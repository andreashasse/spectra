-record(sp_error, {
    location :: [string() | atom()],
    type :: decode_error | type_mismatch | no_match | missing_data | not_matched_fields,
    ctx :: #{
        type => spectra:sp_type_or_ref() | spectra:map_field() | spectra:record_field(),
        value => term(),
        errors => [{spectra:sp_type(), #sp_error{}}],
        message => string(),
        type_args => [{atom(), spectra:sp_type()}],
        err_type => atom(),
        err_reason => any()
    }
}).
