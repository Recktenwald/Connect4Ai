open MathNet.Numerics.LinearAlgebra

let m = matrix [[ 1.0; 2.0 ]
                [ 3.0; 4.0 ]]
let m' = m.Inverse()

[<EntryPoint>]
let main argv =
    printfn "%s" "hello world"