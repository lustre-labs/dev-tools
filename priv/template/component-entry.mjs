import { make_component } from '../dev/javascript/lustre/lustre/runtime/client/component.ffi.mjs';
import { name, {component_name} as component } from '../dev/javascript/{app_name}/{module_path}.mjs';

make_component(component(), name);
