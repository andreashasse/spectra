-module(spectra_only_parameterized_type).

-export_type([item/1, binary_item/0]).

-compile(nowarn_unused_type).

%% Parameterized map type with only [name]. Used to test that apply_only/2
%% correctly recurses into #sp_type_with_variables{} so the filter is applied
%% before type variables are substituted.
-spectra(#{only => [name]}).
-type item(T) :: #{name := T, age := non_neg_integer()}.

%% Concrete instantiation via a user type ref.
-type binary_item() :: item(binary()).
