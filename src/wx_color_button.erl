%%
%%  wx_color_button.erl --
%%
%%     This module create a variant to wxColorPickerCtrl
%%     so we can override the default color chooser and let the user choose
%%     which color picker (s)he wants
%% 
%%  Copyright (c) 2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wx_color_button).
-behaviour(wx_object).
%% Callbacks
-export([init/1, terminate/2, code_change/3, 
	 handle_event/2, handle_cast/2, handle_info/2, 
	 handle_call/3]).

%% API
-export([new/3, getColour/1]).

new(Parent, Id, Opts) ->
    wx_object:start(?MODULE, [Parent, Id, Opts], []).

getColour(Ctrl) ->
    wx_object:call(Ctrl, get_colour).


%% Callbacks

-include_lib("wx/include/wx.hrl").

-record(state, {this, bitmap, brush, bg}).

init([Parent, Id, Opts0]) ->
    Bitmap = wxBitmap:new(60,13),
    Style = proplists:get_value(style, Opts0, ?wxCLRP_DEFAULT_STYLE),
    Opts1 = proplists:delete(style, Opts0),
    DefColor = proplists:get_value(col, Opts1, {0,0,0,0}),
    Opts = proplists:delete(col, Opts1),
    Button = wxBitmapButton:new(Parent, Id, Bitmap, 
				[{style, ?wxBU_AUTODRAW bor Style}|Opts]),
    wxBitmapButton:connect(Button, command_button_clicked),
    
    State = #state{this=Button, bitmap=Bitmap,
		   bg=wxBrush:new(wxWindow:getBackgroundColour(Button)),
		   brush=wxBrush:new(DefColor)},
    update_color(State),
    {Button, State}.
    
update_color(#state{this=This, brush=FG, bg=BG, bitmap=BM}) ->
    DC = wxMemoryDC:new(BM),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBrush(DC, BG),
    wxDC:drawRectangle(DC, {0,0, wxBitmap:getWidth(BM), wxBitmap:getHeight(BM)}),
    wxDC:setBrush(DC, FG),
    wxDC:drawRoundedRectangle(DC, {0,0, wxBitmap:getWidth(BM), wxBitmap:getHeight(BM)}, 4),
    wxMemoryDC:selectObject(DC, ?wxNullBitmap),
    wxMemoryDC:destroy(DC),
    wxBitmapButton:setBitmapLabel(This, BM),
    ok.

handle_event(#wx{event=#wxCommand{type=command_button_clicked}}, 
	     #state{this=This, brush=Brush} = State) ->
    Data = wxColourData:new(),
    wxColourData:setColour(Data, wxBrush:getColour(Brush)),
    wxColourData:setChooseFull(Data, true),
    Dlg = wxColourDialog:new(This, [{data, Data}]),
    wxColourData:destroy(Data),
    case wxDialog:showModal(Dlg) of
	?wxID_CANCEL ->
	    {noreply, State};
	?wxID_OK ->
	    NewData = wxColourDialog:getColourData(Dlg),
	    Col = wxColourData:getColour(NewData),
	    wxBrush:destroy(Brush),
	    Updated = State#state{brush=wxBrush:new(Col)},
	    update_color(Updated),
	    {noreply, Updated}
    end.
	    
handle_call(get_colour, _From, #state{brush=Brush} = State) ->
    {reply, wxBrush:getColour(Brush), State}.

terminate(_Reason, #state{this=_This, brush=Brush}) ->
    wxBrush:destroy(Brush),
    %% wxBitmapButton:destroy(This), %% Is destroyed by the owner
    ok.

handle_cast(_, State) -> State.
handle_info(_, State) -> State.

code_change(_, _, State) -> State.
    
