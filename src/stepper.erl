-module(stepper).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([
         start/2,
         forward/3,
         backward/3,
         position/0
        ]).

%% needed for testing
-export([set_pins/1]).

-record(state, {
       mode = simple,
       pin_states =  {low, low, low, low},
       pin_handles = {nil, nil, nil, nil},
       step_increment = nil,
       position = 0,
       caller = nil,
       timer_message = nil,
       direction = nil,
       steps_remaining = 0,
       end_state = nil
    }).

%% API
-spec start((simple | high_torque | half_step), number()) -> {device, gpio1, ?MODULE, pid(), reference()}.
start(Mode, StepIncrement) when is_atom(Mode) and is_number(StepIncrement) ->
    start_link(#{mode => Mode, step_increment => 1.8}).

-spec forward(number(), number(), (locked | free)) -> done.
forward(Amount, Speed, EndState) when is_number(Amount) and is_number(Speed), is_atom(EndState) ->
    gen_server:call(?MODULE, {move, forward, Amount, Speed, EndState}, infinity).

-spec backward(number(), number(), (locked | free)) -> done.
backward(Amount, Speed, EndState) when is_number(Amount) and is_number(Speed), is_atom(EndState) ->
    gen_server:call(?MODULE, {move, backward, Amount, Speed, EndState}, infinity).

-spec position() -> number().
position() ->
    gen_server:call(?MODULE, position).

%% callbacks
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts) ->
    {ok, TimerMessage} = timed_message:init(),
    State = #state{
        mode = maps:get(mode, Opts),
        step_increment = maps:get(step_increment, Opts),
        pin_handles = init_pin_handles(),
        timer_message = TimerMessage
    },
    {ok, State}.

init_pin_handles() ->
    {
     init_pin_handle(35),
     init_pin_handle(36),
     init_pin_handle(37),
     init_pin_handle(38)
    }.

init_pin_handle(Pin) ->
    gpio:set_pin_mode(Pin, output),
    gpio:digital_write(Pin, low),
    Pin.

handle_call({move, Direction, Amount, Speed, EndState}, From, State = #state{caller = nil, mode = Mode}) ->
    Steps =
        case Mode of
            half_step -> trunc(Amount / State#state.step_increment * 2 + 0.5);
           _ ->          trunc(Amount / State#state.step_increment + 0.5)
        end,
    % when calculating the speed, we are factoring in that the first step has no delay
    StepFrequency = trunc(Speed * (260 / State#state.step_increment)),
    drain_timer_messages(State),
    timed_message:run(StepFrequency),
    NextState =
    State#state{
        caller = From,
        direction = Direction,
        steps_remaining = Steps,
        end_state = EndState
    },
    {noreply, NextState};
handle_call({move, _, _, _, _}, _From, State) ->
    {reply, {error, busy}, State};

handle_call(position, _From, State = #state{mode = half_step}) ->
    {reply, State#state.position * State#state.step_increment * 0.5, State};
handle_call(position, _From, State) ->
    {reply, State#state.position * State#state.step_increment, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(TimerMessage, State = #state{timer_message = TimerMessage}) ->
    NextState = step(State),
    {noreply, NextState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

drain_timer_messages(State = #state{timer_message = TimerMessage}) ->
    receive
        TimerMessage -> drain_timer_messages(State)
    after
        0 -> ok
    end.

step(State = #state{steps_remaining = 0, direction = nil}) ->
    % we should not be stepping so we ignore
    State;
step(State = #state{steps_remaining = 0, end_state = locked, caller = Caller}) ->
    % we finsihed stepping so we let the caller know
    gen_server:reply(Caller, done),
    timed_message:stop(),
    State#state{caller = nil, direction = nil};

step(State = #state{steps_remaining = 0, end_state = free, caller = Caller}) ->
    % we are asked to free the motor so
    % we are releasing all pins but don't persist the state
    % so when we start moving again, the motor does not
    % potentially turn into the wrong direction first
    % (assuming it was not moved by external forces)
    set_pins(State#state{pin_states = {low, low, low, low}}),
    gen_server:reply(Caller, done),
    State#state{caller = nil, direction = nil};

step(State = #state{mode = Mode, position = Position, direction = Direction, steps_remaining = StepsRemaining}) ->
    NextPins = next_pins(Direction, State#state.pin_states, Mode),
    NextPosition =
    case Direction of
         forward  -> Position + 1;
         backward -> Position - 1
    end,
    NextState = State#state{pin_states = NextPins, position = NextPosition, steps_remaining = StepsRemaining - 1},
    set_pins(NextState),
    NextState.

next_pins(Direction, CurrentPins, simple) ->
    case {Direction, CurrentPins} of
        {forward,  {low,  low,  low,  low}}  -> {high, low,  low,  low};
        {forward,  {high, low,  low,  low}}  -> {low,  high, low,  low};
        {forward,  {low,  high, low,  low}}  -> {low,  low,  high, low};
        {forward,  {low,  low,  high, low}}  -> {low,  low,  low,  high};
        {forward,  {low,  low,  low,  high}} -> {high, low,  low,  low};

        {backward, {low,  low,  low,  low}}  -> {low,  low,  low,  high};
        {backward, {high, low,  low,  low}}  -> {low,  low,  low,  high};
        {backward, {low,  high, low,  low}}  -> {high, low,  low,  low};
        {backward, {low,  low,  high, low}}  -> {low,  high, low,  low};
        {backward, {low,  low,  low,  high}} -> {low,  low,  high, low}
    end;
next_pins(Direction, CurrentPins, high_torque) ->
    case {Direction, CurrentPins} of
        {forward,  {low,  low,  low,  low}}  -> {high, high, low,  low};
        {forward,  {high, high, low,  low}}  -> {low,  high, high, low};
        {forward,  {low,  high, high, low}}  -> {low,  low,  high, high};
        {forward,  {low,  low,  high, high}} -> {high, low,  low,  high};
        {forward,  {high, low,  low,  high}} -> {high, high, low,  low};

        {backward, {low,  low,  low,  low}}  -> {high, low, low, high};
        {backward, {high, low,  low,  high}} -> {low, low, high, high};
        {backward, {low,  low,  high, high}} -> {low, high, high, low};
        {backward, {low,  high, high, low}}  -> {high, high, low, low};
        {backward, {high, high, low,  low}}  -> {high, low, low, high}
    end;
next_pins(Direction, CurrentPins, half_step) ->
    case {Direction, CurrentPins} of
        {forward,  {low,  low,  low,  low}}  -> {high, low,  low,  low};
        {forward,  {high, low,  low,  low}}  -> {high, high, low,  low};
        {forward,  {high, high, low,  low}}  -> {low,  high, low,  low};
        {forward,  {low,  high, low,  low}}  -> {low,  high, high, low};
        {forward,  {low,  high, high, low}}  -> {low,  low,  high, low};
        {forward,  {low,  low,  high, low}}  -> {low,  low,  high, high};
        {forward,  {low,  low,  high, high}} -> {low,  low,  low,  high};
        {forward,  {low,  low,  low,  high}} -> {high, low,  low,  high};
        {forward,  {high, low,  low,  high}} -> {high, low,  low,  low};

        {backward, {low,  low,  low,  low}}  -> {low,  low,  low,  high};
        {backward, {low,  low,  low,  high}} -> {low,  low,  high, high};
        {backward, {low,  low,  high, high}} -> {low,  low,  high, low};
        {backward, {low,  low,  high, low}}  -> {low,  high, high, low};
        {backward, {low,  high, high, low}}  -> {low,  high, low,  low};
        {backward, {low,  high, low,  low}}  -> {high, high, low,  low};
        {backward, {high, high, low,  low}}  -> {high, low,  low,  low};
        {backward, {high, low,  low,  low}}  -> {high, low,  low,  high};
        {backward, {high, low,  low,  high}} -> {low,  low,  low,  high}
    end;
next_pins(_Direction, CurrentPins, flicker) ->
    case CurrentPins of
        {low, low, low, low} -> {high, high, high, high};
        {high, high, high, high} -> {low, low, low, low}
    end.

set_pins(#state{pin_handles = {H1, H2, H3, H4}, pin_states = {P1, P2, P3, P4}}) ->
    % io:format("Set pins: ~p ~p ~n", [{H1, H2, H3, H4}, {P1, P2, P3, P4}]),
    gpio:digital_write(H1, P1),
    gpio:digital_write(H2, P2),
    gpio:digital_write(H3, P3),
    gpio:digital_write(H4, P4).

