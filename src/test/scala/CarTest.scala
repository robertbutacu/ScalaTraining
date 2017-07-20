import Car.CarType
import org.scalatest.FunSuite

/**
  * Created by r.butacu on 7/20/2017.
  */
class CarTest extends FunSuite {
  test("Should return sleek"){
    assert(Car.inspect(CarType("Ferrari","LaFerrari",2009,250)) === "Sleek")
  }

  test("Should return Classy"){
    assert(Car.inspect(CarType("Bentley", "Continental GT", 2008, 250)) === "Classy")
  }

  test("Should return 'Murica?"){
    assert(Car.inspect(CarType("Ford Mustang", "GT", 2005, 180)) === "'Murica?")
  }

  test("Should return Retro, love it"){
    assert(Car.inspect( CarType("Camaro", "Z28", 1969, 200)) === "Retro, love it")
  }

  test("Should return Idk"){
    assert(Car.inspect(CarType("Dacia","Sandero",2006,120)) === "Idk")
  }
}
