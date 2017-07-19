/**
  * Created by r.butacu on 7/19/2017.
  */
object Car {

  case class CarType(company: String, model: String, year: Int, topSpeed: Double)

  def inspect(car: CarType): String = {
    car match {
      case CarType("Ferrari", _, _, _) => "Sleek"
      case CarType("Bentley", _, _, _) => "Classy"
      case CarType("Ford Mustang", _, _, _) => "'Murica?"
      case CarType(_, _, year, _) if year < 2000 => "Retro, love it"
      case CarType(_, _, _, _) => "Idk"
    }
  }
}
