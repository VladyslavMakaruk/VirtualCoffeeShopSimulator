package util.exceptions


class OrderParsingException(message: String) extends Exception(message){}

object OrderParsingException {
  private val wrongBeanNameTemplate = "There is no \"%s\" bean type"
  private val wrongCoffeeNameTemplate = "There is no \"%s\" coffee type"
  private val wrongSizeNameTemplate = "There is no \"%s\" size type"
  private val wrongMilkNameTemplate = "There is no \"%s\" milk type"
  private val wrongToppingNameTemplate = "There is no \"%s\" topping type"
  
  def wrongBeanType(beanName:String): OrderParsingException = {
    new OrderParsingException(wrongBeanNameTemplate.format(beanName))
  }
  def wrongCoffeeType(coffeeName: String): OrderParsingException = {
    new OrderParsingException(wrongCoffeeNameTemplate.format(coffeeName))
  }
  def wrongSizeType(sizeName: String): OrderParsingException = {
    new OrderParsingException(wrongSizeNameTemplate.format(sizeName))
  }
  def wrongMilkType(milkName: String): OrderParsingException = {
    new OrderParsingException(wrongMilkNameTemplate.format(milkName))
  }  
  def wrongToppingType(toppingName: String): OrderParsingException = {
    new OrderParsingException(wrongToppingNameTemplate.format(toppingName))
  }  
}