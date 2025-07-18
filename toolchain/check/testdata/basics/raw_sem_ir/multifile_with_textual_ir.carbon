// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// INCLUDE-FILE: toolchain/testing/testdata/min_prelude/none.carbon
// EXTRA-ARGS: --dump-raw-sem-ir --dump-sem-ir-ranges=if-present
//
// Check that we can combine textual IR and raw IR dumping in one compile.
//
// AUTOUPDATE
// TIP: To test this file alone, run:
// TIP:   bazel test //toolchain/testing:file_test --test_arg=--file_tests=toolchain/check/testdata/basics/raw_sem_ir/multifile_with_textual_ir.carbon
// TIP: To dump output, run:
// TIP:   bazel run //toolchain/testing:file_test -- --dump_output --file_tests=toolchain/check/testdata/basics/raw_sem_ir/multifile_with_textual_ir.carbon

// --- a.carbon
package A;

fn A() {}

// --- b.carbon
package B;

import A;

fn B() {
  A.A();
}

// CHECK:STDOUT: ---
// CHECK:STDOUT: filename:        a.carbon
// CHECK:STDOUT: sem_ir:
// CHECK:STDOUT:   import_irs:
// CHECK:STDOUT:     ir0:             {decl_id: inst<none>, is_export: false}
// CHECK:STDOUT:     ir1:             {decl_id: inst<none>, is_export: false}
// CHECK:STDOUT:   import_ir_insts: {}
// CHECK:STDOUT:   name_scopes:
// CHECK:STDOUT:     name_scope0:     {inst: inst14, parent_scope: name_scope<none>, has_error: false, extended_scopes: [], names: {name0: inst15}}
// CHECK:STDOUT:   entity_names:    {}
// CHECK:STDOUT:   functions:
// CHECK:STDOUT:     function0:       {name: name0, parent_scope: name_scope0, call_params_id: inst_block_empty, body: [inst_block5]}
// CHECK:STDOUT:   classes:         {}
// CHECK:STDOUT:   generics:        {}
// CHECK:STDOUT:   specifics:       {}
// CHECK:STDOUT:   struct_type_fields:
// CHECK:STDOUT:     struct_type_fields0: {}
// CHECK:STDOUT:   types:
// CHECK:STDOUT:     'type(TypeType)':
// CHECK:STDOUT:       value_repr:      {kind: copy, type: type(TypeType)}
// CHECK:STDOUT:     'type(Error)':
// CHECK:STDOUT:       value_repr:      {kind: copy, type: type(Error)}
// CHECK:STDOUT:     'type(inst(NamespaceType))':
// CHECK:STDOUT:       value_repr:      {kind: copy, type: type(inst(NamespaceType))}
// CHECK:STDOUT:     'type(inst16)':
// CHECK:STDOUT:       value_repr:      {kind: none, type: type(inst17)}
// CHECK:STDOUT:     'type(inst17)':
// CHECK:STDOUT:       value_repr:      {kind: none, type: type(inst17)}
// CHECK:STDOUT:   insts:
// CHECK:STDOUT:     inst14:          {kind: Namespace, arg0: name_scope0, arg1: inst<none>, type: type(inst(NamespaceType))}
// CHECK:STDOUT:     inst15:          {kind: FunctionDecl, arg0: function0, arg1: inst_block_empty, type: type(inst16)}
// CHECK:STDOUT:     inst16:          {kind: FunctionType, arg0: function0, arg1: specific<none>, type: type(TypeType)}
// CHECK:STDOUT:     inst17:          {kind: TupleType, arg0: inst_block_empty, type: type(TypeType)}
// CHECK:STDOUT:     inst18:          {kind: StructValue, arg0: inst_block_empty, type: type(inst16)}
// CHECK:STDOUT:     inst19:          {kind: Return}
// CHECK:STDOUT:   constant_values:
// CHECK:STDOUT:     values:
// CHECK:STDOUT:       inst14:          concrete_constant(inst14)
// CHECK:STDOUT:       inst15:          concrete_constant(inst18)
// CHECK:STDOUT:       inst16:          concrete_constant(inst16)
// CHECK:STDOUT:       inst17:          concrete_constant(inst17)
// CHECK:STDOUT:       inst18:          concrete_constant(inst18)
// CHECK:STDOUT:     symbolic_constants: {}
// CHECK:STDOUT:   inst_blocks:
// CHECK:STDOUT:     inst_block_empty: {}
// CHECK:STDOUT:     exports:
// CHECK:STDOUT:       0:               inst15
// CHECK:STDOUT:     imports:         {}
// CHECK:STDOUT:     global_init:     {}
// CHECK:STDOUT:     inst_block4:     {}
// CHECK:STDOUT:     inst_block5:
// CHECK:STDOUT:       0:               inst19
// CHECK:STDOUT:     inst_block6:
// CHECK:STDOUT:       0:               inst14
// CHECK:STDOUT:       1:               inst15
// CHECK:STDOUT: ...
// CHECK:STDOUT: --- a.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %A.type: type = fn_type @A [concrete]
// CHECK:STDOUT:   %A: %A.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .A = %A.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %A.decl: %A.type = fn_decl @A [concrete = constants.%A] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @A() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: ---
// CHECK:STDOUT: filename:        b.carbon
// CHECK:STDOUT: sem_ir:
// CHECK:STDOUT:   import_irs:
// CHECK:STDOUT:     ir0:             {decl_id: inst<none>, is_export: false}
// CHECK:STDOUT:     ir1:             {decl_id: inst<none>, is_export: false}
// CHECK:STDOUT:     ir2:             {decl_id: inst15, is_export: false}
// CHECK:STDOUT:   import_ir_insts:
// CHECK:STDOUT:     import_ir_inst0: {ir_id: ir2, inst_id: inst15}
// CHECK:STDOUT:     import_ir_inst1: {ir_id: ir2, inst_id: inst15}
// CHECK:STDOUT:   name_scopes:
// CHECK:STDOUT:     name_scope0:     {inst: inst14, parent_scope: name_scope<none>, has_error: false, extended_scopes: [], names: {name1: inst16, name0: inst17}}
// CHECK:STDOUT:     name_scope1:     {inst: inst16, parent_scope: name_scope0, has_error: false, extended_scopes: [], names: {name1: inst22}}
// CHECK:STDOUT:   entity_names:
// CHECK:STDOUT:     entity_name0:    {name: name1, parent_scope: name_scope1, index: -1, is_template: 0}
// CHECK:STDOUT:   functions:
// CHECK:STDOUT:     function0:       {name: name0, parent_scope: name_scope0, call_params_id: inst_block_empty, body: [inst_block5]}
// CHECK:STDOUT:     function1:       {name: name1, parent_scope: name_scope1}
// CHECK:STDOUT:   classes:         {}
// CHECK:STDOUT:   generics:        {}
// CHECK:STDOUT:   specifics:       {}
// CHECK:STDOUT:   struct_type_fields:
// CHECK:STDOUT:     struct_type_fields0: {}
// CHECK:STDOUT:   types:
// CHECK:STDOUT:     'type(TypeType)':
// CHECK:STDOUT:       value_repr:      {kind: copy, type: type(TypeType)}
// CHECK:STDOUT:     'type(Error)':
// CHECK:STDOUT:       value_repr:      {kind: copy, type: type(Error)}
// CHECK:STDOUT:     'type(inst(NamespaceType))':
// CHECK:STDOUT:       value_repr:      {kind: copy, type: type(inst(NamespaceType))}
// CHECK:STDOUT:     'type(inst18)':
// CHECK:STDOUT:       value_repr:      {kind: none, type: type(inst19)}
// CHECK:STDOUT:     'type(inst19)':
// CHECK:STDOUT:       value_repr:      {kind: none, type: type(inst19)}
// CHECK:STDOUT:     'type(inst24)':
// CHECK:STDOUT:       value_repr:      {kind: none, type: type(inst19)}
// CHECK:STDOUT:   insts:
// CHECK:STDOUT:     inst14:          {kind: Namespace, arg0: name_scope0, arg1: inst<none>, type: type(inst(NamespaceType))}
// CHECK:STDOUT:     inst15:          {kind: ImportDecl, arg0: name1}
// CHECK:STDOUT:     inst16:          {kind: Namespace, arg0: name_scope1, arg1: inst15, type: type(inst(NamespaceType))}
// CHECK:STDOUT:     inst17:          {kind: FunctionDecl, arg0: function0, arg1: inst_block_empty, type: type(inst18)}
// CHECK:STDOUT:     inst18:          {kind: FunctionType, arg0: function0, arg1: specific<none>, type: type(TypeType)}
// CHECK:STDOUT:     inst19:          {kind: TupleType, arg0: inst_block_empty, type: type(TypeType)}
// CHECK:STDOUT:     inst20:          {kind: StructValue, arg0: inst_block_empty, type: type(inst18)}
// CHECK:STDOUT:     inst21:          {kind: NameRef, arg0: name1, arg1: inst16, type: type(inst(NamespaceType))}
// CHECK:STDOUT:     inst22:          {kind: ImportRefLoaded, arg0: import_ir_inst0, arg1: entity_name0, type: type(inst24)}
// CHECK:STDOUT:     inst23:          {kind: FunctionDecl, arg0: function1, arg1: inst_block_empty, type: type(inst24)}
// CHECK:STDOUT:     inst24:          {kind: FunctionType, arg0: function1, arg1: specific<none>, type: type(TypeType)}
// CHECK:STDOUT:     inst25:          {kind: StructValue, arg0: inst_block_empty, type: type(inst24)}
// CHECK:STDOUT:     inst26:          {kind: NameRef, arg0: name1, arg1: inst22, type: type(inst24)}
// CHECK:STDOUT:     inst27:          {kind: Call, arg0: inst26, arg1: inst_block_empty, type: type(inst19)}
// CHECK:STDOUT:     inst28:          {kind: Return}
// CHECK:STDOUT:   constant_values:
// CHECK:STDOUT:     values:
// CHECK:STDOUT:       inst14:          concrete_constant(inst14)
// CHECK:STDOUT:       inst16:          concrete_constant(inst16)
// CHECK:STDOUT:       inst17:          concrete_constant(inst20)
// CHECK:STDOUT:       inst18:          concrete_constant(inst18)
// CHECK:STDOUT:       inst19:          concrete_constant(inst19)
// CHECK:STDOUT:       inst20:          concrete_constant(inst20)
// CHECK:STDOUT:       inst21:          concrete_constant(inst16)
// CHECK:STDOUT:       inst22:          concrete_constant(inst25)
// CHECK:STDOUT:       inst23:          concrete_constant(inst25)
// CHECK:STDOUT:       inst24:          concrete_constant(inst24)
// CHECK:STDOUT:       inst25:          concrete_constant(inst25)
// CHECK:STDOUT:       inst26:          concrete_constant(inst25)
// CHECK:STDOUT:     symbolic_constants: {}
// CHECK:STDOUT:   inst_blocks:
// CHECK:STDOUT:     inst_block_empty: {}
// CHECK:STDOUT:     exports:
// CHECK:STDOUT:       0:               inst17
// CHECK:STDOUT:     imports:
// CHECK:STDOUT:       0:               inst16
// CHECK:STDOUT:       1:               inst22
// CHECK:STDOUT:       2:               inst23
// CHECK:STDOUT:     global_init:     {}
// CHECK:STDOUT:     inst_block4:     {}
// CHECK:STDOUT:     inst_block5:
// CHECK:STDOUT:       0:               inst21
// CHECK:STDOUT:       1:               inst26
// CHECK:STDOUT:       2:               inst27
// CHECK:STDOUT:       3:               inst28
// CHECK:STDOUT:     inst_block6:
// CHECK:STDOUT:       0:               inst14
// CHECK:STDOUT:       1:               inst15
// CHECK:STDOUT:       2:               inst17
// CHECK:STDOUT: ...
// CHECK:STDOUT: --- b.carbon
// CHECK:STDOUT:
// CHECK:STDOUT: constants {
// CHECK:STDOUT:   %B.type: type = fn_type @B [concrete]
// CHECK:STDOUT:   %empty_tuple.type: type = tuple_type () [concrete]
// CHECK:STDOUT:   %B: %B.type = struct_value () [concrete]
// CHECK:STDOUT:   %A.type: type = fn_type @A [concrete]
// CHECK:STDOUT:   %A: %A.type = struct_value () [concrete]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: imports {
// CHECK:STDOUT:   %A: <namespace> = namespace file.%A.import, [concrete] {
// CHECK:STDOUT:     .A = %A.A
// CHECK:STDOUT:     import A//default
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %A.A: %A.type = import_ref A//default, A, loaded [concrete = constants.%A]
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: file {
// CHECK:STDOUT:   package: <namespace> = namespace [concrete] {
// CHECK:STDOUT:     .A = imports.%A
// CHECK:STDOUT:     .B = %B.decl
// CHECK:STDOUT:   }
// CHECK:STDOUT:   %A.import = import A
// CHECK:STDOUT:   %B.decl: %B.type = fn_decl @B [concrete = constants.%B] {} {}
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @B() {
// CHECK:STDOUT: !entry:
// CHECK:STDOUT:   %A.ref.loc6_3: <namespace> = name_ref A, imports.%A [concrete = imports.%A]
// CHECK:STDOUT:   %A.ref.loc6_4: %A.type = name_ref A, imports.%A.A [concrete = constants.%A]
// CHECK:STDOUT:   %A.call: init %empty_tuple.type = call %A.ref.loc6_4()
// CHECK:STDOUT:   return
// CHECK:STDOUT: }
// CHECK:STDOUT:
// CHECK:STDOUT: fn @A [from "a.carbon"];
// CHECK:STDOUT:
