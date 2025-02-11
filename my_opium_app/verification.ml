module PolicyVerifier = struct
  type verification_result = 
    | Valid
    | Invalid of string
    | Error of string

  let verify_permission_consistency policy =
    try
      let all_roles = policy.roles in
      let check_role role =
        List.iter (fun resource ->
          List.iter (fun perm ->
            if not (List.mem perm role.permissions) then
              raise (Invalid_argument 
                (Printf.sprintf 
                  "Role %s missing required permission %s for resource %s"
                  role.name 
                  (string_of_permission perm) 
                  resource.id))
          ) resource.required_permissions
        ) role.resources
      in
      List.iter check_role all_roles;
      Valid
    with
    | Invalid_argument msg -> Invalid msg
    | e -> Error (Printexc.to_string e)

  let verify_mutual_exclusion policy =
    let check_constraint = function
      | MutualExclusion (role1, role2) ->
          List.iter (fun r1_perm ->
            List.iter (fun r2_perm ->
              if r1_perm = r2_perm then
                raise (Invalid_argument 
                  (Printf.sprintf 
                    "Mutual exclusion violation: roles %s and %s share permission %s"
                    role1.name 
                    role2.name 
                    (string_of_permission r1_perm)))
            ) role2.permissions
          ) role1.permissions
      | _ -> ()
    in
    try
      List.iter check_constraint policy.constraints;
      Valid
    with
    | Invalid_argument msg -> Invalid msg
    | e -> Error (Printexc.to_string e)
end
