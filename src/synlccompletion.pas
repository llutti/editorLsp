unit SynLCCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLProc,
  SynCompletion;

type

  { TSynLCCompletion }

  TSynLCCompletion = class(TSynCompletion)
    private
      function GetItemListCount: integer;
    protected
    public
      procedure AddItem(s: String; s1:String = '');
      procedure ClearItens;
    published
      property ItemListCount:integer read GetItemListCount;
  end;

implementation

{ TSynLCCompletion }

function TSynLCCompletion.GetItemListCount: integer;
begin
  result := 0;
end;

procedure TSynLCCompletion.AddItem(s: String; s1: String);
begin

end;

procedure TSynLCCompletion.ClearItens;
begin

end;

end.

