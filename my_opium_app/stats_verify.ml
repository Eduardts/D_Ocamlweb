(* src/stats_verify.ml *)
module StatsVerifier = struct
  type property = 
    | NonNegativeMean
    | OrderedQuartiles
    | ValidStdDev
    | OutlierBounds

  let verify_statistics stats properties =
    try
      List.iter (function
        | NonNegativeMean ->
            if stats.mean < 0. then
              raise (Invalid_argument "Mean cannot be negative")
        | OrderedQuartiles ->
            let (q1, q2, q3) = stats.quartiles in
            if not (q1 <= q2 && q2 <= q3) then
              raise (Invalid_argument "Quartiles must be ordered")
        | ValidStdDev ->
            if stats.std_dev < 0. then
              raise (Invalid_argument "Standard deviation must be non-negative")
        | OutlierBounds ->
            let (q1, _, q3) = stats.quartiles in
            let iqr = q3 -. q1 in
            let lower_bound = q1 -. 1.5 *. iqr in
            let upper_bound = q3 +. 1.5 *. iqr in
            List.iter (fun x ->
              if x < lower_bound || x > upper_bound then
                raise (Invalid_argument "Outlier outside acceptable bounds")
            ) stats.outliers
      ) properties;
      Valid
    with
    | Invalid_argument msg -> Invalid msg
    | e -> Error (Printexc.to_string e)
end

