//Functions
val nonEmpty: String => String | NoAddress = s => 
    if (s.trim.isEmpty) NoAddress() else s

val validateAddress: String => Address | InvalidAddress = {
    case "CO"          => Address(state="CO")
    case parseError    => InvalidAddress(parseError)
}

val ship: Address => ShippingTag | ShippingError = address =>
  if (address.state == "Hawai") ShippingError("Not delivering there yet")
  else ShippingTag(address)

