module Example exposing (conversionTests, pixelValueCalcTests, testDownsample)

import Array as A
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


conversionTests : Test
conversionTests =
    test "conversion to gray"
        (\_ ->
            Expect.equal
                (Main.convertToGray (A.map toFloat (A.initialize 123456 identity)))
                (A.map ((*) 4) (A.map toFloat (A.initialize (123456 // 4) identity)))
        )


pixelValueCalcTests : Test
pixelValueCalcTests =
    test "pixel value calculation"
        (\_ ->
            Expect.equal (round <| Main.calcPixelVal 100 3 9696 testRawImg) 255
        )


testDownsample : Test
testDownsample =
    test "downsample of image"
        (\_ ->
            Expect.equal (Main.downsampleImg testRawImg) testMnistImg
        )


testRawImg : A.Array Float
testRawImg =
    A.repeat 10000 255 |> A.set 9696 0 |> A.set 9697 0 |> A.set 9698 0 |> A.set 9796 0 |> A.set 9797 0 |> A.set 9798 0 |> A.set 9896 0 |> A.set 9897 0 |> A.set 9898 0


testMnistImg : A.Array Float
testMnistImg =
    A.set 783 255 (A.repeat 784 0)
