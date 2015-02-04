%% ===================================================================
%% Conversion helper macros
%% ===================================================================

-define(i2l(I), integer_to_list(I)).
-define(l2a(I), list_to_atom(I)).
-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2f(L), list_to_float(L)).
-define(i2b(I), list_to_binary(integer_to_list(I))).
-define(b2a(B), binary_to_atom(B, utf8)).
-define(b2ea(B), binary_to_existing_atom(B, utf8)).
-define(b2i(B), list_to_integer(binary_to_list(B))).
-define(b2f(B), list_to_float(binary_to_list(B))).
-define(b2l(B), binary_to_list(B)).
-define(a2b(A), list_to_binary(atom_to_list(A))).
-define(a2l(A), atom_to_list(A)).
-define(a2i(A), list_to_integer(atom_to_list(A))).
-define(f2b(F), list_to_binary(float_to_list(F))).
-define(io2b(Io), iolist_to_binary(Io)).
