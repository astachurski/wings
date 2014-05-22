%%
%%  ww_color_slider.erl --
%%
%%     A color slider
%%
%%  Copyright (c) 2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(ww_color_slider).
-behaviour(wx_object).
%% Callbacks
-export([init/1, terminate/2, code_change/3,
	 handle_event/2, handle_cast/2, handle_info/2,
	 handle_call/3]).

%% API
-export([new/3, new/4, getColor/1, setColor/2]).

-compile(export_all).

new(Parent, Id, Col) ->
    new(Parent, Id, Col, []).
new(Parent, Id, Col, Opts) ->
    wx_object:start(?MODULE, [Parent, Id, Col, Opts], []).

getColor(Ctrl) ->
    wx_object:call(Ctrl, get_color).

setColor(Ctrl, RGB) ->
    wx_object:cast(Ctrl, {set_color, RGB}).

%% Callbacks

-include_lib("wx/include/wx.hrl").

-record(state, {this, pos,
		c1, c2,
		bmp, bgb}).
-define(PANEL_MIN_SIZE, {100, 20}).
-define(SLIDER_MIN_HEIGHT, 10).
-define(SLIDER_OFFSET, {8, 5}).

-define(wxGC, wxGraphicsContext).

init([Parent, Id, {R,G,B}, Opts0]) ->
    {Style0, Opts} = default(style, 0, Opts0),
    Style =  Style0 bor ?wxFULL_REPAINT_ON_RESIZE bor ?wxCLIP_CHILDREN,
    Panel = wxPanel:new(Parent, [{winid, Id}, {style, Style}|Opts]),
    wxWindow:setMinSize(Panel, ?PANEL_MIN_SIZE),
    Bmp = slider_bitmap(),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, erase_background), %% WIN32 only?
    BGC = wxPanel:getBackgroundColour(Panel),
    Brush = wxBrush:new(BGC),
    wxPanel:connect(Panel, left_down),
    wxPanel:connect(Panel, left_up),

    {Hue,S,V} = rgb_to_hsv(R, G, B),
    SCol = hsv_to_rgb(Hue, S, 0.0),
    ECol = hsv_to_rgb(Hue, S, 1.0),

    {Panel, #state{this=Panel, pos=V,
		   c1=SCol, c2=ECol,
		   bmp=Bmp, bgb=Brush}}.

handle_sync_event(#wx{obj=Panel}, _Obj,
		  #state{this=Panel, pos=X,
			 c1=C1, c2=C2,
			 bmp=Bmp, bgb=BGB}) ->
    io:format("se~n",[]),
    DC = case os:type() of
	     {win32, _} -> %% Flicker on windows
		 %%BDC = wx:typeCast(wxBufferedPaintDC:new(Panel), wxPaintDC),
		 BDC = wxPaintDC:new(Panel),
		 wxDC:setBackground(BDC, BGB),
		 wxDC:clear(BDC),
		 BDC;
	     _ ->
		 wxPaintDC:new(Panel)
	 end,
    try
	{_,_, W0, _} = wxPanel:getRect(Panel),
	{X0,Y0} = ?SLIDER_OFFSET,
	wxDC:gradientFillLinear(DC, {X0, Y0, W0-2*X0, ?SLIDER_MIN_HEIGHT},
				rgb256(C1), rgb256(C2), [{nDirection, ?wxRIGHT}]),
	Pos = X0 + (W0-2*X0)*X,
	wxDC:drawBitmap(DC, Bmp, {trunc(Pos-7),0})
    catch _:Reason ->
	    io:format("se result ~p ~p~n",[Reason, erlang:get_stacktrace()])
    end,
    wxPaintDC:destroy(DC),
    io:format("se end~n",[]),
    ok.

handle_event(#wx{event=#wxMouse{type=motion, x=X}},
	     #state{this=This} = State) ->
    io:format("~p~n",[?LINE]),
    {noreply, State#state{pos=slider_pos(This, X)}};

handle_event(#wx{event=#wxMouse{type=left_down, x=X}},
	     #state{this=This} = State) ->
    io:format("~p~n",[?LINE]),
    wxPanel:captureMouse(This),
    wxPanel:connect(This, motion),
    {noreply, State#state{pos=slider_pos(This, X)}};
handle_event(#wx{event=#wxMouse{type=left_up}},
	     #state{this=This} = State) ->
    wxPanel:disconnect(This, motion),
    wxPanel:releaseMouse(This),
    {noreply, State};
handle_event(#wx{event=#wxErase{}}, State) ->
    %% io:format("Skip Ev ~p~n",[Ev]),
    {noreply, State}.

handle_call(get_color, _From, #state{c1={H,S,_}, pos=V} = State) ->
    Color = hsv_to_rgb(H,S,V),
    {reply, Color, State}.

handle_cast({set_color, {R,G,B}}, State = #state{this=This}) ->
    {Hue,S,V} = rgb_to_hsv(R, G, B),
    SCol = hsv_to_rgb(Hue, S, 0.0),
    ECol = hsv_to_rgb(Hue, S, 1.0),
    io:format("~p~n",[?LINE]),
    wxWindow:refresh(This),
    {noreply, State#state{pos=V, c1=SCol, c2=ECol}}.

terminate(_Reason, #state{this=_This, bmp=Bmp, bgb=BGB}) ->
    wxBrush:destroy(BGB),
    wxBitmap:destroy(Bmp),
    %% wxPanel:destroy(This), %% Is destroyed by the owner
    ok.

handle_info(_, State) -> State.

code_change(_, _, State) -> State.

default(Key, Def, Opts) ->
    {proplists:get_value(Key, Opts, Def),
     proplists:delete(Key,Opts)}.

slider_pos(This, X) ->
    wxWindow:refresh(This),
    {W, _} = wxPanel:getSize(This),
    {X0,_Y0} = ?SLIDER_OFFSET,
    max(0.0, min(1.0, (X-X0)/(W-X0*2))).

rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R, G, B).

rgb_to_hsv(R,G,B) ->
    {H,S,V} = wings_color:rgb_to_hsv(R,G,B),
    {round(H),S,V}.

hsv_to_rgb({H,S,V}) ->
    hsv_to_rgb(H, S, V).

hsv_to_rgb(H, S, V) ->
    wings_color:hsv_to_rgb(H, S, V).

rgb256({R,G,B}) -> {round(R*255),round(G*255),round(B*255)};
rgb256({R,G,B,_A}) -> {round(R*255),round(G*255),round(B*255)}.

%% Image / icon data

slider_bitmap() ->
    I = wxImage:new(15, 20, rgb()), %alpha(), [{static_data, false}]), doesn't work...
    wxImage:setAlpha(I, alpha()),
    Bmp = wxBitmap:new(I),
    wxImage:destroy(I),
    Bmp.

rgb() ->
    <<0,0,0,0,0,0,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,0,0,0,0,0,0,0,0,0,240,240,240,193,193,193,141,141,141,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,141,141,141,193,193,193,240,240,240,240,240,240,0,0,0,0,0,0,240,240,240,141,141,141,221,221,221,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,221,221,221,134,134,134,232,232,232,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,252,252,252,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,252,252,252,112,112,112,211,211,211,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,251,251,251,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,251,251,251,112,112,112,198,198,198,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,251,251,251,240,240,240,240,240,240,0,0,0,240,240,240,240,240,240,240,240,240,251,251,251,112,112,112,195,195,195,240,240,240,0,0,0,240,240,240,240,240,240,112,112,112,251,251,251,239,239,239,239,239,239,239,239,239,0,0,0,239,239,239,239,239,239,251,251,251,112,112,112,195,195,195,250,250,250,0,0,0,0,0,0,240,240,240,112,112,112,251,251,251,238,238,238,238,238,238,0,0,0,0,0,0,238,238,238,238,238,238,251,251,251,112,112,112,188,190,190,250,250,250,0,0,0,0,0,0,240,240,240,112,112,112,250,250,250,236,236,236,236,236,236,0,0,0,0,0,0,236,236,236,236,236,236,250,250,250,112,112,112,188,190,190,255,255,255,0,0,0,0,0,0,231,234,234,112,112,112,250,250,250,235,235,235,235,235,235,0,0,0,235,235,235,235,235,235,235,235,235,250,250,250,112,112,112,188,190,190,255,255,255,0,0,0,0,0,0,252,252,252,112,112,112,246,246,246,219,219,219,219,219,219,0,0,0,0,0,0,219,219,219,219,219,219,246,246,246,112,112,112,188,190,190,255,255,255,0,0,0,0,0,0,240,240,240,112,112,112,245,245,245,217,217,217,217,217,217,0,0,0,0,0,0,217,217,217,217,217,217,245,245,245,112,112,112,195,195,195,255,255,255,0,0,0,0,0,0,240,240,240,112,112,112,245,245,245,215,215,215,215,215,215,0,0,0,0,0,0,215,215,215,215,215,215,245,245,245,112,112,112,195,195,195,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,245,245,245,218,218,218,214,214,214,214,214,214,214,214,214,214,214,214,218,218,218,245,245,245,112,112,112,195,195,195,240,240,240,0,0,0,0,0,0,240,240,240,165,165,165,182,182,182,244,244,244,217,217,217,212,212,212,212,212,212,217,217,217,244,244,244,182,182,182,114,114,114,200,200,200,240,240,240,0,0,0,0,0,0,240,240,240,225,225,225,155,155,155,180,180,180,244,244,244,215,215,215,215,215,215,244,244,244,180,180,180,104,104,104,149,149,149,216,216,216,240,240,240,0,0,0,0,0,0,240,240,240,240,240,240,222,222,222,151,151,151,180,180,180,243,243,243,243,243,243,180,180,180,105,105,105,145,145,145,205,205,205,234,234,234,240,240,240,0,0,0,0,0,0,240,240,240,240,240,240,240,240,240,222,222,222,151,151,151,178,178,178,180,180,180,106,106,106,145,145,145,205,205,205,234,234,234,240,240,240,240,240,240,0,0,0,0,0,0,0,0,0,240,240,240,240,240,240,240,240,240,224,224,224,158,158,158,138,138,138,160,160,160,205,205,205,234,234,234,240,240,240,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,240,240,240,240,240,240,234,234,234,223,223,223,222,222,222,234,234,234,240,240,240,0,0,0,0,0,0,0,0,0,0,0,0>>.


alpha() ->
    <<0,0,171,188,186,176,198,224,226,226,227,242,132,0,0,0,168,252,255,255,255,255,255,255,255,255,255,219,33,0,0,184,255,255,255,255,255,255,255,255,255,255,253,126,0,0,184,255,255,255,249,182,200,251,255,255,255,255,119,0,0,184,255,255,255,130,5,12,118,255,255,255,255,142,0,0,184,254,255,255,5,0,1,16,255,255,255,255,140,0,4,184,244,255,255,5,1,0,47,255,255,255,254,127,0,0,184,244,255,255,13,0,0,51,255,255,255,255,119,0,0,184,254,255,255,11,0,0,32,255,255,255,254,119,0,0,184,254,255,255,44,0,1,40,255,255,255,255,119,0,0,184,244,255,255,44,0,0,16,255,255,255,255,119,0,0,184,243,255,255,51,0,0,27,255,255,255,255,119,0,0,184,244,255,255,93,0,0,58,255,255,255,255,119,0,0,119,254,255,255,162,1,1,159,255,255,255,255,119,0,0,130,255,255,255,247,167,165,246,255,255,255,255,124,0,0,119,184,255,255,255,255,255,255,255,255,255,255,119,0,0,0,119,184,255,255,255,255,255,255,255,254,184,0,0,0,2,0,119,184,255,255,255,255,255,253,217,0,22,0,0,0,5,0,119,184,255,255,253,226,155,17,0,0,0,0,0,0,0,2,119,184,255,184,94,3,0,0,0,0>>.


test() ->
    Me = self(),
    process_flag(trap_exit, true),
    spawn_link(fun() -> run_test(Me) end),
    receive Msg -> Msg end.

run_test(_Parent) ->
    Frame = wxDialog:new(wx:new(), -1, "FOO"),
    Panel = wxPanel:new(Frame),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sz, wxButton:new(Panel, 42, [{label, "A button"}])),
    wxSizer:add(Sz, wxStaticText:new(Panel, 43, "Some static text")),
    wxSizer:add(Sz, wxSlider:new(Panel, 46, 27, 1, 100), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, 45, {72/255, 255/255, 23/255}, []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, wxButton:new(Panel, 44, [{label, "B button"}])),
    wxPanel:setSizerAndFit(Panel, Sz),
    wxSizer:setSizeHints(Sz, Frame),
    wxDialog:showModal(Frame),
    ok.

