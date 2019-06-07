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

