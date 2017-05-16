-define(make_record_to_map(Record),
    fun(Val) ->
        Fields = record_info(fields, Record),
        [_Tag|Values] = tuple_to_list(Val),
        maps:from_list(lists:zip(Fields, Values))
    end).

