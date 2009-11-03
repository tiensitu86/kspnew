unit SpkXMLParser;

interface

uses SysUtils, Classes, Math, KSPStrings;

const
  LINE_BREAK = #13;

type
  ParseException = class(Exception)
  private
    FPosition: integer;
  protected
  public
    constructor Create(AMessage: string; APosition: integer); reintroduce;
    property Position: integer Read FPosition Write FPosition;
  published
  end;

type
  TSpkXMLParameter = class(TObject)
  private
    FAttribute, FValue: string;
  protected
  public
    constructor Create; overload;
    constructor Create(AAttribute, Avalue: string); overload;
    destructor Destroy; override;
    property Attribute: string Read FAttribute Write FAttribute;
    property Value: string Read FValue Write FValue;
  published
  end;

type
  TSpkXMLParameters = class(TObject)
  private
    FList: TList;
  protected
    function GetParamByName(index: string; autocreate: boolean): TSpkXMLParameter;
    function GetParamByIndex(index: integer): TSpkXMLParameter;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AParameter: TSpkXMLParameter); overload;
    procedure Add(Value, Attribute: string); overload;
    procedure Delete(index: integer);
    procedure Clear;

    property ParamByName[index: string;
      autocreate: boolean]: TSpkXMLParameter Read GetParamByName; default;
    property ParamByIndex[index: integer]: TSpkXMLParameter Read GetParamByIndex;
    property Count: integer Read GetCount;
  published
  end;

type
  TSpkXMLNode = class(TObject)
  private
    FParameters: TSpkXMLParameters;
    FList: TList;
    FName: string;
    FText: string;
  protected
    function GetSubNodeByName(index: string; autocreate: boolean): TSpkXMLNode;
    function GetSubNodeByIndex(index: integer): TSpkXMLNode;

    function GetCount: integer;
  public
    constructor Create; overload;
    constructor Create(Aname: string); overload;
    destructor Destroy; override;

    procedure Add(ANode: TSpkXMLNode);
    procedure Delete(index: integer);
    procedure Clear;

    property SubNodeByName[index: string;
      autocreate: boolean]: TSpkXMLNode Read GetSubNodeByName; default;
    property SubNodeByIndex[index: integer]: TSpkXMLNode Read GetSubNodeByIndex;

    property Parameters: TSpkXMLParameters Read FParameters;
    property Name: string Read FName Write FName;
    property Text: string Read FText Write FText;
    property Count: integer Read GetCount;
  published
  end;

type
  TSpkXMLParser = class(TObject)
  private
    FList: TList;
  protected
    function GetNodeByName(index: string; autocreate: boolean): TSpkXMLNode;
    function GetNodeByIndex(index: integer): TSpkXMLNode;
    function Build(node: TSpkXMLNode; indent: integer): string;
    function ParseNode(s: string; var position: integer): TSpkXMLNode;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ANode: TSpkXMLNode);
    procedure Delete(index: integer);
    procedure Clear;

    function XMLString: string;
    procedure SaveToFile(filename: string);
    procedure LoadFromFile(filename: string);
    procedure Parse(s: string);

    property NodeByName[index: string;
      autocreate: boolean]: TSpkXMLNode Read GetNodeByName; default;
    property NodeByIndex[index: integer]: TSpkXMLNode Read GetNodeByIndex;
  published
  end;

function Slash(s: string): string;
function UnSlash(s: string): string;

implementation

{ Utilities }

uses KSPConstsVars;

function IntToStr3(i: integer): string;

begin
  Result := IntToStr(i);
  while length(Result) < 3 do
    Result := '0' + Result;
end;

function Slash(s: string): string;

begin
  Result := s;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#039;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;

function Unslash(s: string): string;

begin
  Result := s;
  Result := StringReplace(Result, '&#039;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

{ TParseException }

constructor ParseException.Create(AMessage: string; APosition: integer);

begin
  self.Message := AMessage;
  FPosition    := APosition;
end;

{ TSpkXMLParameter }

constructor TSpkXMLParameter.Create;

begin
  inherited Create;
  FAttribute := '';
  FValue     := '';
end;

constructor TSpkXMLParameter.Create(AAttribute, Avalue: string);

begin
  inherited Create;
  FAttribute := AAttribute;
  FValue     := AValue;
end;

destructor TSpkXMLParameter.Destroy;

begin
  inherited Destroy;
end;

{ TSpkXMLParameters }

function TSpkXMLParameters.GetParamByName(index: string;
  autocreate: boolean): TSpkXMLParameter;

var
  i:   integer;
  pom: TSpkXMLParameter;

begin
  i := 0;
{$B-}
  while (i < FList.Count) and (uppercase(TSpkXMLParameter(FList[i]).Attribute) <>
      uppercase(index)) do
    Inc(i);
  if i < FList.Count then
    Result := TSpkXMLParameter(FList[i])
  else
  begin
    if not (autocreate) then
      Result := nil
    else
    begin
      pom := TSpkXMLParameter.Create(index, '');
      FList.add(pom);
      Result := pom;
    end;
  end;
end;

function TSpkXMLParameters.GetParamByIndex(index: integer): TSpkXMLParameter;

begin
  if (index < 0) or (index >= FList.Count) then
    raise Exception.Create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
  //'TSpkXMLParameters.GetParamByIndex: Incorrect index ('+inttostr(index)+')');
  Result := TSpkXMLParameter(FList[index]);
end;

function TSpkXMLParameters.GetCount: integer;

begin
  Result := FList.Count;
end;

constructor TSpkXMLParameters.Create;

begin
  inherited Create;
  FList := TList.Create;
end;

destructor TSpkXMLParameters.Destroy;

begin
  while FList.Count > 0 do
    Delete(0);
end;

procedure TSpkXMLParameters.Add(AParameter: TSpkXMLParameter);

begin
  FList.add(AParameter);
end;

procedure TSpkXMLParameters.Add(Value, Attribute: string);
var
  AParameter: TSpkXMLParameter;
begin
  AParameter := TSpkXMLParameter.Create(Attribute, Value);
  Add(Aparameter);
end;

procedure TSpkXMLParameters.Delete(index: integer);

begin
  if (index < 0) or (index >= FList.Count) then
    raise Exception.Create(Format(sXMLIncorrectIndex, [IntToStr(index)]));

  TSpkXMLParameter(FList[index]).Free;
  FList.Delete(index);
end;

procedure TSpkXMLParameters.Clear;

begin
  while FList.Count > 0 do
    Delete(0);
end;

{ TSpkXMLNode }

function TSpkXMLNode.GetSubNodeByName(index: string;
  autocreate: boolean): TSpkXMLNode;

var
  i:   integer;
  pom: TSpkXMLNode;

begin
  i := 0;
{$B-}
  while (i < FList.Count) and (uppercase(TSpkXMLNode(FList[i]).Name) <> uppercase(index)) do
    Inc(i);
  if i < FList.Count then
    Result := TSpkXMLNode(FList[i])
  else
  begin
    if not (autocreate) then
      Result := nil
    else
    begin
      pom := TSpkXMLNode.Create(index);
      FList.add(pom);
      Result := pom;
    end;
  end;
end;

function TSpkXMLNode.GetSubNodeByIndex(index: integer): TSpkXMLNode;

begin
  if (index < 0) or (index >= FList.Count) then
    raise Exception.Create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
  Result := TSpkXMLNode(FList[index]);
end;

function TSpkXMLNode.GetCount: integer;

begin
  Result := FList.Count;
end;

constructor TSpkXMLNode.Create;

begin
  inherited Create;
  FParameters := TSpkXMLParameters.Create;
  FList := TList.Create;
  FName := '';
  FText := '';
end;

constructor TSpkXMLNode.Create(Aname: string);

begin
  inherited Create;
  FParameters := TSpkXMLParameters.Create;
  FList := TList.Create;
  FName := AName;
  FText := '';
end;

destructor TSpkXMLNode.Destroy;

begin
  FParameters.Free;
  while FList.Count > 0 do
    Delete(0);
  inherited Destroy;
end;

procedure TSpkXMLNode.Add(ANode: TSpkXMLNode);

begin
  FList.add(ANode);
end;

procedure TSpkXMLNode.Delete(index: integer);

begin
  if (index < 0) or (index >= FList.Count) then
    raise Exception.Create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
  TSpkXMLNode(FList[index]).Free;
  FList.Delete(index);
end;

procedure TSpkXMLNode.Clear;

begin
  while FList.Count > 0 do
    Delete(0);
end;

{ TSpkXMLParser }

function TSpkXMLParser.GetNodeByName(index: string; autocreate: boolean): TSpkXMLNode;

var
  i:   integer;
  pom: TSpkXMLNode;

begin
  i := 0;
{$B-}
  while (i < FList.Count) and (uppercase(TSpkXMLNode(FList[i]).Name) <> uppercase(index)) do
    Inc(i);
  if i < FList.Count then
    Result := TSpkXMLNode(FList[i])
  else
  begin
    if not (autocreate) then
      Result := nil
    else
    begin
      pom := TSpkXMLNode.Create(index);
      FList.add(pom);
      Result := pom;
    end;
  end;
end;

function TSpkXMLParser.GetNodeByIndex(index: integer): TSpkXMLNode;

begin
  if (index < 0) or (index >= FList.Count) then
    raise Exception.Create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
  Result := TSpkXMLNode(FList[index]);
end;

function TSpkXMLParser.Build(node: TSpkXMLNode; indent: integer): string;

var
  i: integer;

  function MkIndent(i: integer): string;

  begin
    Result := '';
    while length(Result) < i do
      Result := ' ' + Result;
  end;

begin
  Result := '';
  if (node.Text = '') and (node.Count = 0) then
  begin
    Result := Result + MkIndent(indent) + '<' + node.Name;
    if node.Parameters.Count > 0 then
      for i := 0 to node.parameters.Count - 1 do
      begin
        Result := Result + ' ' + node.parameters.ParamByIndex[i].Attribute;
        Result := Result + '="' + Slash(node.parameters.ParamByIndex[i].Value) + '"';
      end;
    Result := Result + '/>' + LINE_BREAK;
  end
  else
  begin
    Result := Result + MkIndent(indent) + '<' + node.Name;

    // Parametry

    if node.Parameters.Count > 0 then
      for i := 0 to node.parameters.Count - 1 do
      begin
        Result := Result + ' ' + node.parameters.ParamByIndex[i].Attribute;
        Result := Result + '="' + Slash(node.parameters.ParamByIndex[i].Value) + '"';
      end;
    Result := Result + '>';

    // Tekst

    if node.Text <> '' then
      Result := Result + Slash(node.Text) + LINE_BREAK
    else
      Result := Result + LINE_BREAK;

    // Podgałęzie

    if node.Count > 0 then
      for i := 0 to node.Count - 1 do
        Result := Result + Build(node.SubNodeByIndex[i], indent + 2);

    // Domknięcie

    Result := Result + MkIndent(indent) + '</' + node.Name + '>' + LINE_BREAK;
  end;
end;

function TSpkXMLParser.ParseNode(s: string; var position: integer): TSpkXMLNode;

var
  s1, s2: string;
  i:      integer;
  node, subnode: TSpkXMLNode;
  len:    integer;
  Quote:  char;
  b:      boolean;

begin
{$B-}
  node := nil;
  len  := length(s);
  try
    // Znak "<"

    // Oczekiwany znak "<"
    if s[position] <> '<' then
      raise ParseException.Create(sXMLIncorrectCharLower, position);
    //'Oczekiwany znak "<"',position);
    Inc(position);

    // Plik nie może się tu zakończyć.
    if position > len then
      raise ParseException.Create(sXMLIncorrectEOF, position);
    //'Nieoczekiwany koniec pliku - oczekiwano definicji znacznika',position);

    // Oczekiwany znacznik otwierający - nie może wystąpić znak '/'
    if s[position] = '/' then
      raise ParseException.Create(sXMLUnexpectedCloseTag, position);
    //'Nieoczekiwany znacznik zamykający',position);

    // Nazwa gałęzi
    s1 := '';
    while (position <= len) and (not (s[position] in [#9, ' ', '>', '/'])) and (s1 <> '!--') do
    begin
      if s[position] in [#0..#31] then
        raise ParseException.Create(sXMLIncorrectTagName, position);
      //'Niewłaściwy znak w nazwie znacznika',position);
      s1 := s1 + s[position];
      Inc(position);
    end;

    // Dwa przypadki - komentarz lub nie
    if s1 = '!--' then
    begin
      // Szukanie końca komentarza
      while (position <= len) and (copy(s, position, 3) <> '-->') do
        Inc(position);
      if position > len then
        raise ParseException.Create(sXMLNoEOC, position);
      //'Nieoczekiwany koniec pliku - oczekiwano końca komentarza',position);
      Inc(position, 3);
    end
    else
    begin
      node := TSpkXMLNode.Create(s1);
      // Ewentualne parametry
      repeat
        // Dowolna ilość spacji lub tabulacji
        while (position <= len) and (s[position] in [' ', #9]) do
          Inc(position);
        if position > len then
          raise ParseException.Create(sXMLIncorrectEOFParams, position);
        //'Nieoczekiwany koniec pliku - oczekiwano parametrów lub zakończenia znacznika',position);

        if not (s[position] in ['/', '>']) then
        begin
          // Nazwa parametru, po niej dowolna ilość spacji lub tabulacji
          s1 := '';
          while (position <= len) and (not (s[position] in [#9, ' ', '='])) do
          begin
            if s[position] in [#0..#31, '/', '>', '''', '"'] then
              raise ParseException.Create(
                ('sXMLIncorrectCharParamName'), position);
            //'Nieprawidłowy znak w nazwie parametru',position);
            s1 := s1 + s[position];
            Inc(position);
          end;
          while (position <= len) and (s[position] in [' ', #9]) do
            Inc(position);

          if (position > len) then
            raise ParseException.Create(('sXMLIncorrectEOFEqual'), position);
          //'Nieoczekiwany koniec pliku - oczekiwano znaku "="',position);

          // znak = i dowolna ilość spacji lub tabulacji
          if s[position] <> '=' then
            raise ParseException.Create(('sXMLIncorrectCharEqual'), position);
          //'Nieprawidłowy znak - oczekiwano "="',position);
          Inc(position);
          while (position <= len) and (s[position] in [' ', #9]) do
            Inc(position);
          if (position > len) then
            raise ParseException.Create(('sXMLIncorrectEOFB'), position);
          //'Nieoczekiwany koniec pliku - oczekiwano znaku " lub ''',position);

          // znak " lub '
          if not (s[position] in ['"', '''']) then
            raise ParseException.Create(('sXMLIncorrectCharB'), position);
          //'Nieprawidłowy znak - oczekiwano " lub ''',position);
          quote := s[position];
          Inc(position);

          // Treść parametru zakończona odpowiednim ogranicznikiem w zależności od DoubleQuote
          s2 := '';
          while (position <= len) and (s[position] <> quote) do
          begin
            //                  if s[position] in [#0..#31] then
            //                     raise ParseException.create('Nieprawidłowy znak',position);
            s2 := s2 + s[position];
            Inc(position);
          end;
          if (position > len) then
            raise ParseException.Create(('sXMLIncorrectEOFNoParam'), position);
          //'Nieoczekiwany koniec pliku - oczekiwano domknięcia parametru',position);
          node.Parameters.Add(TSpkXMLParameter.Create(s1, Unslash(s2)));

          Inc(position);
          if (position > len) then
            raise ParseException.Create(('sXMLIncorrectEOF'), position);
          //'Nieoczekiwany koniec pliku - oczekiwano parametru lub domknięcia znacznika',position);
        end;
      until s[position] in ['/', '>'];

      // Dwie możliwości - samozamykający się znacznik (/) lub koniec
      // znacznika otwierającego (>)
      if s[position] = '/' then
      begin
        // Znak >
        Inc(position);
        if (position > len) then
          raise ParseException.Create(('sXMLIncorrectEOF'), position);
        //'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika ">"',position);
        if s[position] <> '>' then
          raise ParseException.Create(('sXMLIncorrectChar'), position);
        //'Nieprawidłowy znak - oczekiwano domknięcia znacznika ">"',position);
        Inc(position);
      end
      else
      begin
        // Nie może tu być końca pliku - aż do znacznika domykającego
        Inc(position);
        if position > len then
          raise ParseException.Create(('sXMLIncorrectEOF'), position);
        //'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika '+node.name,position);

        // No i teraz niewielka rzeźnia, czyli tekst i podznaczniki.
        s2 := ''; // Przechowuje tekst znacznika
        s1 := '';
        while (position <= len) and (s[position] <> '<') do
        begin
          if s[position] in [#10, #13] then
          begin
            s2 := s2 + s1;
            s1 := s[position];
            Inc(position);
            if position <= len then
              if s[position] in [#10, #13] then
              begin
                s1 := s1 + s[position];
                Inc(position);
              end;
          end
          else
          begin
            s1 := s1 + s[position];
            Inc(position);
          end;
        end;
        // Jeśli s1 jest postaci <enter>[spacje], nie dodawaj go do zawartości tekstu
        b := True; // Fragment wyrównujący
        if length(s1) > 1 then
          for i := 1 to length(s1) do
          begin
            if (i = 1) and not (s1[i] in [#10, #13]) then
              b := False
            else
            if (i = 2) and not (s1[i] in [#10, #13, ' ', #9]) then
              b := False
            else
            if (i > 2) and not (s1[i] in [' ', #9]) then
              b := False;
          end;
        if not (b) then
          s2 := s2 + s1;

        node.Text := Unslash(s2);

        // Oczekiwane podznaczniki lub znacznik zamykający - pomiędzy nimi
        // dowolna ilość znaków białych (spacja, enter, tabulator)
        b := False;
        repeat
          if (position > len) or (position + 1 > len) then
            raise ParseException.Create(
              ('sXMLIncorrectNoClosingTag') + node.Name, position);
          //'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika '+node.name,position);
          if s[position + 1] = '/' then
            b := True
          else
          begin
            // Parsuj podgałąź
            subnode := ParseNode(s, position);
            if subnode <> nil then //komentarz
              node.Add(subnode);

            // Usuń pozostałe znaki białe aż do wystąpienia kolejnego znaku "<"
            while (position <= len) and (s[position] in [' ', #9, #10, #13]) do
              Inc(position);

            if (position > len) then
              raise ParseException.Create(('sXMLIncorrectEOF'), position);
            //'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika '+node.name,position);

            if s[position] <> '<' then
              raise ParseException.Create(('sXMLIncorrectChar'), position);
            //'Nieprawidłowy znak - tekst znacznika musi znaleźć się przed podznacznikami',position);
          end
        until b;

        // Osiągnięto </ - sprawdzamy, czy zgadza się nazwa znacznika otwierającego i zamykającego
        Inc(position, 2);

        // Dopuszczam znaki białe pomiędzy </ i nazwą znacznika
        while (position <= len) and (s[position] in [' ', #9]) do
          Inc(position);

        if (position > len) then
          raise ParseException.Create(('sXMLIncorrectEOF'), position);
        //'Nieoczekiwany koniec pliku - oczekiwano nazwy domykanego znacznika (w domyśle: '+node.name+')',position);

        s1 := '';
        while (position <= len) and not (s[position] in ['>', ' ', #9]) do
        begin
          if s[position] in [#0..#31] then
            raise ParseException.Create(('sXMLIncorrectChar'), position);
          //'Niewłaściwy znak w nazwie znacznika domykającego!',position);
          s1 := s1 + s[position];
          Inc(position);
        end;
        if uppercase(s1) <> uppercase(node.Name) then
          raise ParseException.Create(('sXMLIncorrectChar'), position);
        //'Znacznik otwierający nie odpowiada znacznikowi domykającemu, odpowiednio: "'+node.name+'" i "'+s1+'"',position);

        while (position <= len) and (s[position] in [' ', #9]) do
          Inc(position);

        if position > len then
          raise ParseException.Create(('sXMLIncorrectEOF'), position);
        //'Nieoczekiwany koniec pliku - oczekiwano znaku ">"',position);
        if s[position] <> '>' then
          raise ParseException.Create(('sXMLIncorrectChar'), position);
        //'Nieprawidłowy znak - oczekiwano ">"',position);

        // Zakończono parsowanie znacznika XML. Ustawiamy pozycję na znak zaraz
        // po sparsowanym znaczniku.
        Inc(position);
      end;
    end;

  except
    if node <> nil then
      node.Free;
    raise;
  end;

  Result := node;
end;

constructor TSpkXMLParser.Create;

begin
  inherited Create;
  FList := TList.Create;
end;

destructor TSpkXMLParser.Destroy;

begin
  while FList.Count > 0 do
    Delete(0);
  FList.Free;
  inherited Destroy;
end;

procedure TSpkXMLParser.Add(ANode: TSpkXMLNode);

begin
  FList.add(ANode);
end;

function TSpkXMLParser.XMLString: string;

var
  i: integer;

begin
  Result := '';
  if FList.Count > 0 then
    for i := 0 to FList.Count - 1 do
      Result := Result + Build(TSpkXMLNode(FList[i]), 0);
end;

procedure TSpkXMLParser.Delete(index: integer);

begin
  if (index < 0) or (index >= FList.Count) then
    raise Exception.Create(Format(('sXMLIncorrectIndex'), [IntToStr(index)]));
  TSpkXMLNode(FList[index]).Free;
  FList.Delete(index);
end;

procedure TSpkXMLParser.Clear;

begin
  while FList.Count > 0 do
    Delete(0);
end;

procedure TSpkXMLParser.SaveToFile(filename: string);

var
  fs: TStringList;

begin
  fs      := TStringList.Create;//(filename,fmCreate or fmShareDenyWrite);
  fs.Text := self.XMLString;
  fs.SaveToFile(filename);
  fs.Free;
end;

procedure TSpkXMLParser.LoadFromFile(filename: string);

var
  s:  string;
  fs: TFileStream;

begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  setlength(s, fs.Size);
  fs.Read(s[1], fs.size);
  fs.Free;
  Clear;
  Parse(s);
end;

procedure TSpkXMLParser.Parse(s: string);

var
  i:    integer;
  node: TSpkXMLNode;
  len:  integer;
  line, ch: integer;

  procedure CharToLineChar(s: string; Ach: integer; var line: integer;
  var ch: integer);

  begin
    line := 1;
    ch   := Ach;
    while ((ch > min(pos(#13, s), pos(#10, s))) and (min(pos(#13, s), pos(#10, s)) > 0)) do
    begin
      Inc(line);
      ch := ch - min(pos(#13, s), pos(#10, s));
      System.Delete(s, 1, min(pos(#13, s), pos(#10, s)));
      if s[1] in [#13, #10] then
      begin
        Dec(ch);
        system.Delete(s, 1, 1);
      end;
    end;
  end;

begin
{$B-}
  try
    self.Clear;
    i   := 1;
    len := length(s);

    while i <= len do
    begin
      while (i <= len) and (s[i] in [' ', #9, #10, #13]) do
        Inc(i);
      if (i <= len) then
      begin
        if (s[i] = '<') or (s[i] = '?') then
        begin
          node := ParseNode(s, i);
          if node <> nil then
            Add(node);
        end
        else
          raise ParseException.Create(('sXMLIncorrectCharLower'), i);
      end;
    end;

  except
    on e: ParseException do
    begin
      CharToLineChar(s, e.position, line, ch);
      raise Exception.Create(Format(
        ('sXMLError'), [IntToStr(Line), IntToStr(ch), e.Message]));
      //'Wystąpił błąd podczas parsowania, linia '+inttostr(line)+' znak '+inttostr(ch)+', komunikat:'+#13+e.message);
    end;
  end;

end;

end.
