(* src/statistics.ml *)
module Statistics = struct
  type dataset = {
    name: string;
    data: float array;
    metadata: (string * string) list;
  }

  type analysis_result = {
    mean: float;
    median: float;
    std_dev: float;
    quartiles: float * float * float;
    outliers: float list;
  }

  let calculate_statistics dataset =
    let sorted = Array.copy dataset.data |> Array.sort compare in
    let len = Array.length sorted in
    let mean = Array.fold_left (+.) 0. sorted /. float_of_int len in
    let median = 
      if len mod 2 = 0 
      then (sorted.(len/2) +. sorted.(len/2 - 1)) /. 2.
      else sorted.(len/2)
    in
    let variance = 
      Array.fold_left (fun acc x ->
        let diff = x -. mean in
        acc +. diff *. diff
      ) 0. sorted /. float_of_int len
    in
    {
      mean;
      median;
      std_dev = sqrt variance;
      quartiles = (
        sorted.(len / 4),
        median,
        sorted.(3 * len / 4)
      );
      outliers = []  (* Implement outlier detection *)
    }
end

