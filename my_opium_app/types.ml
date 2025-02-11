type permission = 
  | Read 
  | Write 
  | Execute
  | Admin

type resource = {
  id: string;
  path: string;
  required_permissions: permission list;
}

type role = {
  name: string;
  permissions: permission list;
  resources: resource list;
}

type policy = {
  id: string;
  roles: role list;
  constraints: constraint_rule list;
}

and constraint_rule = 
  | TimeBasedAccess of time_constraint
  | LocationBased of location_constraint
  | MutualExclusion of role * role

and time_constraint = {
  start_time: float;
  end_time: float;
  timezone: string;
}

and location_constraint = {
  allowed_ips: string list;
  allowed_regions: string list;
}


