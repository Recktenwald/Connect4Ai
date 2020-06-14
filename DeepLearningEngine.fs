module NeuralNetEngine

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random

(* let m = matrix [[ 1.0; 2.0 ]
                [ 3.0; 4.0 ]]
let m' = m.Inverse()
let v = vector [1.0;2.0]
m*v
 *)
(*
#r @"C:/Users/rener/.nuget/packages/mathnet.numerics.fsharp/4.11.0/lib/net45/MathNet.Numerics.FSharp.dll";;
#r @"C:\Users\rener\.nuget\packages\mathnet.numerics\4.11.0\lib\net461\MathNet.Numerics.dll"
#r @"C:\Users\rener\.nuget\packages\mathnet.numerics.fsharp\4.11.0\lib\net45\MathNet.Numerics.FSharp.dll"
The second one does the trick. Then I can Access the SPecial functions. And the @ seems to be important as well
 *)

type Layer =
    { WeightMat: Matrix<float>
      ActFun: float -> float }

type Network = Layer list
type Node = float

type Nodelst =
    { RawNodes: Vector<Node>
      ActNodes: Vector<Node> }

// We are stroing the weightings of the bias in the weightMatrix hence (i+1)
// and assume that we are always just picking 1 as bias
let initNetwork (sizes: int list) (actFun: float -> float): Network =
    let n = List.length sizes
    [ for (i, j) in (List.zip (List.tail sizes) sizes.[0..n - 2]) ->
        { WeightMat = DenseMatrix.random<double> (j + 1) i (ContinuousUniform(-1.0, 1.0))
          ActFun = actFun } ]

let inputSize (network: Network) = network.[0].WeightMat.ColumnCount

// lastItem and outputSize should probably not be used

let rec lastItem (l: 'a list): 'a =
    match l with
    | [] -> failwith "Empty List"
    | [ x ] -> x
    | x :: xs -> lastItem xs

let outputSize (network: Network) = (lastItem network).WeightMat.RowCount

let vecAppend (vec: Vector<float>) x =
    CreateVector.Dense(Array.append (vec.ToArray()) [| x |])

let computeViaNetwork network vec =
    List.fold (fun neurons (layer: Layer) -> (layer.WeightMat) * (vecAppend neurons 1.0)) vec network
(* CreateVector.Dense([|1.0;2.0;3.0|])
Vector<double> [|1;2|] *)




// List.fold (fun s x -> s+x) "" ["H";"E";"L";"L";"O"]
