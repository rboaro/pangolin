unit cloudComputing.HTTP.Example;

interface

uses
  System.SysUtils, System.JSON,
  System.Net.HttpClientComponent, System.Net.URLClient,
  System.Net.HttpClient, System.Classes;

 type
   TTodoClass = class
   public
     class procedure getTodo(Ret: TStrings);
   end;

implementation

{ TTodoClass }

class procedure TTodoClass.getTodo(Ret: TStrings);
var
  Client: TNetHTTPClient;
  Resp: IHTTPResponse;
  Obj: TJSONObject;
begin
  if not assigned(Ret) then exit;

  Client := TNetHTTPClient.Create(nil);
  try
    Resp := Client.Get('https://jsonplaceholder.typicode.com/todos/1');
    Ret.Add('HTTP: ' + Resp.StatusCode.ToString);

    Obj := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8)) as TJSONObject;
    try
      Ret.Add('title = ' +  Obj.GetValue<string>('title'));
      Ret.Add('completed = ' + Obj.GetValue<Boolean>('completed').ToString);
    finally
      Obj.Free;
    end;
  finally
    Client.Free;
  end;
end;



end.
