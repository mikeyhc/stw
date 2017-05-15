-record(stw_entry, {title :: binary(),
                    date :: calendar:datetime(),
                    author :: binary(),
                    path :: binary()
                   }).
-type stw_entry() :: #stw_entry{}.
