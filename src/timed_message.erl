-module(timed_message).
-include("ledc.hrl").

-export([init/0, run/1, stop/0, set_frequency/1]).

init() ->
    listen(2).

run(Freq) ->
    init_pwm(1, Freq).

stop() ->
    ledc:stop(?LEDC_HIGH_SPEED_MODE, ?LEDC_CHANNEL_0, 0).

listen(DetectionPin) ->
    io:format("Set up listener.~n"),
    GPIO = gpio:open(),
    gpio:set_direction(GPIO, DetectionPin, input),
    ok = gpio:set_int(GPIO,  DetectionPin, rising),
    io:format("Done.~n"),
    {ok, {gpio_interrupt, DetectionPin}}.

init_pwm(PWMPin, Freq) ->
    io:format("Init PWM with pin ~p @ ~p Hz ~n", [PWMPin, Freq]),
    Channel = ?LEDC_CHANNEL_0,
    ledc:timer_config([
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, Freq},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {timer_num, ?LEDC_TIMER_0}
    ]),

    %% bind pin to this timer in a channel
    ledc:channel_config([
        {channel, Channel},
        {duty, 0},
        {gpio_num, PWMPin},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER_0}
    ]),
    ledc:set_duty(?LEDC_HIGH_SPEED_MODE, Channel, 4095),
    ledc:update_duty(?LEDC_HIGH_SPEED_MODE, Channel),
    ok.

set_frequency(Freq) ->
    io:format("Set PWM to ~p Hz ~n", [Freq]),
    ledc:set_freq(?LEDC_HIGH_SPEED_MODE, ?LEDC_TIMER_0, Freq).


