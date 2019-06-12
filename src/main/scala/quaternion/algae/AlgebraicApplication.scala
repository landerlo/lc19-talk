package quaternion.algae
import scala.language.implicitConversions
import scala.reflect._

/*
 * Exploration of a leaner FP syntax using type disjunction and something I'm
 * calling "Algebraic application", a more flexible function application where if
 * we have f: A => B and  x: (X | A | Z) we can apply to get (X | B | Z) 
 *
 * We apply with the reverse apply, or pipe symbol, |>
 * We have two variants, |> which is biased to match the first type
 * or |~|> that can match any type but doesn't consume the input type from the output type
 * The syntax below shows possibilities or more expressive function compositions
 * but the approach is not practical in the dotty encoding as we need to reify 
 * functions and values through classtags and reflection. With a combinatorial complexity of
 * cases of different disjunctions.
 *
 * Apart from all the caveats of higher order kinds and reflection, disjunctions are not checked
 * to be truly disjunctive, i.e orthogonal. The matching relies on this property
 *
 * by Lander Lopez
 */
object AlgebraicApplicationExample  extends App {
    import AlgebraicApplication._

    /* Domain for a Address capturing validation and shipment tag creation
    * Process is: String -> addressValidation -> shippingTagCreation
    * Happy path types in the flow are String -> Address -> ShippingTag
    * Whole process type including validation: 
    * String -> ShippingTag | ShippingError | InvalidAddress | NoAddress
    */
    case class Address(state: String)
    case class ShippingTag(addr: Address)

    case class ShippingError(err: String)
    case class InvalidAddress(raw: String)
    case class NoAddress()

    //Functions
    val nonEmptyP: String => String | NoAddress = s => if (s.trim.isEmpty) NoAddress() else s
    val nonEmpty: RFun2[String, String, NoAddress] = nonEmptyP

    val validateAddressP: String => (Address | InvalidAddress) = {
        case "CO"          => Address(state="CO")
        case parseError    => InvalidAddress(parseError)
    }
    val validateAddress: RFun2[String, Address, InvalidAddress] = validateAddressP

    val shipP: Address => (ShippingTag | ShippingError) = address => 
        if (address.state == "Hawai") ShippingError("Not delivering there yet")
        else ShippingTag(address)

    val ship: RFun2[Address, ShippingTag, ShippingError] = shipP

    // For brevity we can alias the errors 
    type Errors = ShippingError | InvalidAddress | NoAddress
    
    val failed: ShippingTag | Errors = "ColoraDo, the Sunny State"  |> nonEmpty |> validateAddress |> ship
    println(failed) //InvalidAddress(ColoraDo, the Sunny State)

    val ok: ShippingTag | Errors = "CO"  |> nonEmpty |> validateAddress |> ship
    println(ok) // ShippingTag(Address(CO))

    // We can combine the piping into a function
    val process: String => (ShippingTag | Errors) = (_: String) |> nonEmpty |> validateAddress |> ship

    /* We maintain all the possible errors in the type.  
     *  val okForgotOneError: ShippingTag | ShippingError | NoAddress  = "ColoraDo, the Sunny State"  |> nonEmpty |> validateAddress |> ship
     * doesn't compile: type doesn't include error InvalidAddress
     */
    
    //The flexible matching and error preservation allows for more focused error recovery
    val inferAddressP: InvalidAddress => (Address | InvalidAddress) = (ia: InvalidAddress) => 
        if (ia.raw.toUpperCase.contains("CO")) Address("CO") else ia

    val inferAddress: RFun2[InvalidAddress, Address, InvalidAddress] = inferAddressP 

    // Apply to a subset of the types, independent of the position            

    val invalidAddress = "ColoraDo, the Sunny State"  |> nonEmpty |> validateAddress
    val invalidAddressUnreified: Address | InvalidAddress | NoAddress = invalidAddress
    println(invalidAddressUnreified) // InvalidAddress(ColoraDo, the Sunny State)

    val recovered: Address | InvalidAddress | NoAddress = invalidAddress |~|> inferAddress
    println(recovered) // Address(CO)

    /* Note how the inferAddress match to a relevan type regardess of the type's place
     * disjunction. For that |~\> is used, as opposed to |> this pipe can match any type but
     * it doesn't consume the type, that's why InvalidAddress need to remain in the type even though
     * it's never going to be there as it would have been matched.
     */
    
    //All these applications are type safe, if there is no possible match won't compile
    val recoverWrong: String => Address | InvalidAddress = Address(_)
    val recoverWrongR: RFun2[String, Address, InvalidAddress] = recoverWrong

   // invalidAddress |~|> recoverWrong   //Doesn't compile

}
object AlgebraicApplication {

    type CT = ClassTag

    /* 
     * Reified functions. we reify the signature to keep type information and know
     * if the domain -the target of the function, B in A=>B- has a disjunction
     * We cover the cases of no disjunction (RFun1) or an either (RFun2)
     */

    case class RFun1[A: CT, B: CT](f: A => B)
    case class RFun2[A: CT, B1: CT, B2: CT](f: A => (B1 | B2))

    //Reification of functions into RFuns. The inference need fully specified types. To be completely transparent, 
    // implicits for every possible type signature would need to be provided
    implicit def rf1[A: CT, B: CT]         (f: A => B        ): RFun1[A, B]      = RFun1(f)
    implicit def rf2[A: CT, B1: CT, B2: CT](f: A => (B1 | B2)): RFun2[A, B1, B2] = RFun2(f)

    // Reification of the disjunctions RD1 is A RD2 is A | B, RD3: A | B | C ....
    // We need to multiply the length of the disjunction times the disjunction on the return types of functions.
    case class RD1[D1: CT](a: D1) {
        def |>[B: CT](rf1: RFun1[D1, B]): RD1[B] = RD1[B](rf1.f(a))
        def |>[B1: CT, B2: CT](rf2: RFun2[D1, B1, B2]): RD2[B1, B2] = RD2[B1, B2](rf2.f(a))
    }

   case class RD2[D1:CT, D2: CT](a: (D1 | D2)) {
        def |>[B: CT](rf1: RFun1[D1, B]): RD2[B, D2] = a match {
            case d1: D1 => RD2[B, D2](rf1.f(d1))
            case d2: D2 => RD2[B, D2](a.asInstanceOf[B | D2])
        }
        def |>[B: CT, C: CT, D <: (D1 | D2)](rf2: RFun2[D1, B, C]): RD3[B, C, D2] = a match {
                case d1: D1 => RD3[B, C, D2](rf2.f(d1))
                case d2: D2 => RD3[B, C, D2](a.asInstanceOf[B | C | D2])
            }
        def |~|>[A: CT, B: CT](rf1: RFun1[A, B])(implicit ev: A <:< (D1 | D2)): RD3[B, D1, D2] = a match {
            case x: A => RD3[B, D1, D2](rf1.f(x))
            case _    => RD3[B, D1, D2](a.asInstanceOf[B | D1 | D2])
        }
        def |~|>[A <: (D1 | D2): CT, B: CT, C: CT](rf2: RFun2[A, B, C]): RD4[B, C, D1, D2] = a match {
                case d1: D1 if subClass[D1, A] => RD4[B, C, D1, D2](rf2.f(d1.asInstanceOf[A]))
                case d2: D2 if subClass[D2, A]=> RD4[B, C, D1, D2](rf2.f(d2.asInstanceOf[A]))
                case _     => RD4[B, C, D1, D2](a.asInstanceOf[B | C | D1 | D2])
            }
    }

    case class RD3[D1: CT, D2: CT, D3: CT](a: (D1 | D2 | D3)) {
        def |>[B: CT](rf1: RFun1[D1, B]): RD3[B, D2, D3] = a match {
            case d1: D1 => RD3[B, D2, D3](rf1.f(d1))
            case _ => RD3[B, D2, D3](a.asInstanceOf[B | D2 | D3])
        }
        def |>[B: CT, C: CT, D <: (D1 | D2 | D3)](rf2: RFun2[D1, B, C]): RD4[B, C, D2, D3] = a match {
             case d1: D1 => RD4[B, C, D2, D3](rf2.f(d1))
             case _      => RD4[B, C, D2, D3](a.asInstanceOf[B | C | D2 | D3])
         }
        def |~|>[A <: (D1 | D2): CT, B: CT](rf1: RFun1[A, B]): RD4[B, D1, D2, D3] = a match {
             case d1: D1 if subClass[D1, A] => RD4[B, D1, D2, D3](rf1.f(d1.asInstanceOf[A]))
             case d2: D2 if subClass[D2, A] => RD4[B, D1, D2, D3](rf1.f(d2.asInstanceOf[A]))
             case d3: D3 if subClass[D3, A] => RD4[B, D1, D2, D3](rf1.f(d3.asInstanceOf[A]))
             case _    => RD4[B, D1, D2, D3](a.asInstanceOf[B | D1 | D2 | D3])
        }
         def |~|>[A <: (D1 | D2 | D3): CT, B: CT, C: CT](rf2: RFun2[A, B, C]): RD5[B, C, D1, D2, D3] = a match {
             case d1: D1 if subClass[D1, A] => RD5[B, C, D1, D2, D3](rf2.f(d1.asInstanceOf[A]))
             case d2: D2 if subClass[D2, A] => RD5[B, C, D1, D2, D3](rf2.f(d2.asInstanceOf[A]))
             case d3: D3 if subClass[D3, A] => RD5[B, C, D1, D2, D3](rf2.f(d3.asInstanceOf[A]))
             case _    => RD5[B, C, D1, D2, D3](a.asInstanceOf[B | C | D1 | D2 | D3])
         }
   }
   case class RD4[D1: CT, D2: CT, D3: CT, D4: CT](a: (D1 | D2 | D3 | D4)) {
         //...
    }
    case class RD5[D1: CT, D2: CT, D3: CT, D4: CT, D5: CT](a: (D1 | D2 | D3 | D4 | D5)) {
       // ...
    }
    case class RD6[D1: CT, D2: CT, D3: CT, D4: CT, D5: CT, D6: CT](a: (D1 | D2 | D3 | D4 | D5 | D6)) {
       // ...
    }


    implicit def reifyValue[A: CT](a: A): RD1[A] = RD1(a)
    
    implicit def unreify1[A: CT](rv: RD1[A]): A = rv.a
    implicit def unreify2[D1: CT, D2: CT](rv: RD2[D1, D2]): (D1 | D2) = rv.a
    implicit def unreify3[D1: CT, D2: CT, D3: CT](rv: RD3[D1, D2, D3]): (D1 | D2 | D3) = rv.a
    implicit def unreify4[D1: CT, D2: CT, D3: CT, D4: CT](rv: RD4[D1, D2, D3, D4]): (D1 | D2 | D3 | D4) = rv.a
    implicit def unreify5[D1: CT, D2: CT, D3: CT, D4: CT, D5: CT](rv: RD5[D1, D2, D3, D4, D5]): (D1 | D2 | D3 | D4 | D5) = rv.a
    implicit def unreify6[D1: CT, D2: CT, D3: CT, D4: CT, D5: CT, D6: CT](rv: RD6[D1, D2, D3, D4, D5, D6]): (D1 | D2 | D3 | D4 | D5 | D6) = rv.a

    def subClass[A: CT, B: CT]: Boolean = 
        implicitly[CT[A]].runtimeClass.isAssignableFrom(implicitly[CT[B]].runtimeClass)
 
}