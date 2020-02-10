-ifndef(FUNBOX_H).

-define(FUNBOX_H, true).
-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).
-define(REDIS_DB, 7).
-define(REDIS_GENERATOR_QUEUE, "generated").
-define(REDIS_SIMPLE_NUMBERS_SET, "filtered").
-define(UPPER_VALUE, 20).

-define(WORKER(Name), #{id => Name, start => {Name, start_link, []}}).

-endif.
