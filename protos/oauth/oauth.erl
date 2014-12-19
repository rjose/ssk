-module(oauth).

-export([get_access_token/1]).

get_access_token(RequestToken) ->
	io:format("Making request with ~p~n", [RequestToken]),
	Method = post,
	URL = "https://www.googleapis.com/oauth2/v3/token",
	%URL = "http://localhost:8000",
	Header = [],
	Type = "application/x-www-form-urlencoded",
	ClientId = "173156479681-981h8uit78nfgkkhtuk842jb2t6v6oae.apps.googleusercontent.com",
	ClientSecret = "",
	Body = io_lib:format("grant_type=authorization_code&code=~s&client_id=~s&client_secret=~s&redirect_uri=urn:ietf:wg:oauth:2.0:oob", [RequestToken, ClientId, ClientSecret]),
	
	HTTPOptions = [],
	Options = [],
	FlatBody = lists:flatten(Body),
	
	io:format("Body: ~s", [FlatBody]),
	_Result = httpc:request(Method, {URL, Header, Type, FlatBody}, HTTPOptions, Options).
