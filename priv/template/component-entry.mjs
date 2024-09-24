import { make_lustre_client_component } from '../dev/javascript/lustre/lustre.ffi.mjs';
import { name, {component_name} as component } from '../dev/javascript/{app_name}/{module_path}.mjs';

make_lustre_client_component(component(), name);
