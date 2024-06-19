module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
    
  describe "Condiciones de viaje" $ do
    it "cualquierViaje siempre retorna True" $ do
      cualquierViaje (Viaje lucas (20, 4, 2017) 100) `shouldBe` True

    it "viajesDeMasDe200pe solo retorna True para viajes de más de $200" $ do
      viajesDeMasDe200pe (Viaje lucas (20, 4, 2017) 150) `shouldBe` False
      viajesDeMasDe200pe (Viaje lucas (20, 4, 2017) 250) `shouldBe` True

    it "viajesClienteNletras solo retorna True para viajes de clientes con más de n letras" $ do
      viajesClienteNletras 4 (Viaje lucas (20, 4, 2017) 100) `shouldBe` True
      viajesClienteNletras 5 (Viaje lucas (20, 4, 2017) 100) `shouldBe` False

    it "viajesFueraDeZona solo retorna True para viajes de clientes que no viven en una zona determinada" $ do
      viajesFueraDeZona "Victoria" (Viaje lucas (20, 4, 2017) 100) `shouldBe` False
      viajesFueraDeZona "Rosario" (Viaje lucas (20, 4, 2017) 100) `shouldBe` True

  describe "Funciones de negocio" $ do
    it "choferPuedeTomarViaje verifica si un chofer puede tomar un viaje" $ do
      choferPuedeTomarViaje (Viaje lucas (20, 4, 2017) 100) daniel `shouldBe` True
      choferPuedeTomarViaje (Viaje lucas (20, 4, 2017) 100) alejandra `shouldBe` True

    it "liquidacionChofer suma los costos de cada uno de los viajes" $ do
      liquidacionChofer daniel `shouldBe` 150
      liquidacionChofer alejandra `shouldBe` 0

    it "efectuarViaje agrega un viaje a la lista de viajes del chofer" $ do
      length (viajes (choferConMenosViajes (efectuarViaje (Viaje lucas (20, 4, 2017) 100) [daniel, alejandra]))) `shouldBe` 1

  describe "Nito Infy" $ do
    it "Nito Infy puede tomar un viaje con Lucas" $ do
      choferPuedeTomarViaje (Viaje lucas (2, 5, 2017) 500) nitoInfy `shouldBe` True
