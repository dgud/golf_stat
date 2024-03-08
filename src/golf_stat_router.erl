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
           [{"/login", {golf_stat_main_controller, login_view}, #{methods => [get]}},
            {"/assets/[...]", "assets"}
           ]
      },

     #{prefix => "",
       security => {gs_auth, login},
       routes =>
           [
            {"/", {golf_stat_main_controller, index}, #{methods => [get]}},
            {"/", {golf_stat_main_controller, index}, #{methods => [post]}},
            {"/courses", {golf_stat_main_controller, courses_view}, #{methods => [get]}},
            {"/course/:courseId", {golf_stat_main_controller, course_view}, #{methods => [get]}},
            {"/add_course", {golf_stat_main_controller, add_course_view}, #{methods => [get]}},
            {"/add_round", {golf_stat_main_controller, add_user_round_view}, #{methods => [get]}},
            {"/text_stats", {golf_stat_main_controller, user_stats_string_view}, #{methods => [get]}},
            {"/diagram_stats", {golf_stat_main_controller, user_stats_diagram_view}, #{methods => [get]}},
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
            {"/user/stats_string",      {golf_stat_main_controller, get_stats_string}, #{methods => [options, post]}},
            {"/user/stats_diagram",     {golf_stat_main_controller, get_stats_diagram}, #{methods => [options, post]}},
            {"/user/stats_string/:nr",  {golf_stat_main_controller, get_stats_string}, #{methods => [options, get]}},
            {"/user/stats_diagram/:nr", {golf_stat_main_controller, get_stats_diagram}, #{methods => [options, get]}}
           ]}
    ].
