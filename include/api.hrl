-define(API, [            % mock ( No::integer(), [Methods,Address,Headers,Type,Req]) -> HTTP RESPONSE
               mocks/0,   % mocks() -> Num::integer().
               pipe/0,    % pipe()  -> { { Module::atom(), Endpoint::list() },
                          %            [ { Pack::atom(), Unpack::atom() }, Retry::integer(), Address::list() } | _ ] }.
               headers/1, % headers(Acc::proplists()) -> Headers::proplists().
               host/1,    % host(Bank::list())        -> Endpoint::list().
               test/0     % test()  -> Results::list().
            ]).

