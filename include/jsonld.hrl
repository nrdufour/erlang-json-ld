%%
%%
%%

-record(triple, {
    type     :: resource | literal,
    subject  :: binary(),
    object   :: binary(),
    property :: binary()
}).

