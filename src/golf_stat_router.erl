-module(golf_stat_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [
     #{
       prefix => "",
       security => false,
       routes =>
           [{"/login", {golf_stat_main_controller, login_view}, #{methods => [get]}}
            %% {"/login", {gs_auth, login}, #{methods => [post]}}
           ]
      },

     #{prefix => "",
       security => {gs_auth, login},
       routes =>
           [
            {"/", {golf_stat_main_controller, index}, #{methods => [get]}},
            {"/", {golf_stat_main_controller, index}, #{methods => [post]}},
            {"/assets/[...]", "assets"}
           ]
      },
     #{prefix => "/api/json",
       security => {gs_auth, verify_login},
       plugins => [
                   {pre_request, nova_request_plugin, #{decode_json_body => true}}
                  ],
       routes =>
           [
            {"/courses",            {golf_stat_main_controller, get_courses}, #{methods => [options, get]}},
            {"/course/:courseId",   {golf_stat_main_controller, get_course}, #{methods => [options, get]}},
            {"/add_course",         {golf_stat_main_controller, add_course}, #{methods => [options, post]}},
            {"/user/add_user",      {golf_stat_main_controller, add_user}, #{methods => [options, post]}},
            {"/user/selections",    {golf_stat_main_controller, get_user_selections}, #{methods => [options, get]}},
            {"/user/add_round",     {golf_stat_main_controller, add_user_round}, #{methods => [options, post]}},
            {"/user/stats_string",  {golf_stat_main_controller, get_stats_string}, #{methods => [options, post]}},
            {"/user/stats_diagram", {golf_stat_main_controller, get_stats_diagram}, #{methods => [options, post]}}
           ]}
    ].