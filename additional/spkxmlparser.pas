unit SpkXMLParser;

interface

uses sysutils, classes, math, KSPStrings;

const LINE_BREAK = #13;

type ParseException = class(Exception)
     private
       FPosition : integer;
     protected
     public
       constructor create(AMessage : string; APosition : integer); reintroduce;
       property Position : integer read FPosition write FPosition;
     published
     end;

type TSpkXMLParameter = class(TObject)
     private
       FAttribute,
       FValue : string;
     protected
     public
       constructor create; overload;
       constructor create(AAttribute, Avalue : string); overload;
       destructor destroy; override;
       property Attribute : string read FAttribute write FAttribute;
       property Value : string read FValue write FValue;
     published
     end;

type TSpkXMLParameters = class(TObject)
     private
       FList : TList;
     protected
       function GetParamByName(index : string; autocreate : boolean) : TSpkXMLParameter;
       function GetParamByIndex(index : integer) : TSpkXMLParameter;
       function GetCount : integer;
     public
       constructor create;
       destructor destroy; override;

       procedure Add(AParameter : TSpkXMLParameter); overload;
       procedure Add(Value, Attribute: string); overload;
       procedure Delete(index : integer);
       procedure Clear;

       property ParamByName[index : string; autocreate : boolean] : TSpkXMLParameter read GetParamByName; default;
       property ParamByIndex[index : integer] : TSpkXMLParameter read GetParamByIndex;
       property Count : integer read GetCount;
     published
     end;

type TSpkXMLNode = class(TObject)
     private
       FParameters : TSpkXMLParameters;
       FList : TList;
       FName : string;
       FText : string;
     protected
       function GetSubNodeByName(index : string; autocreate : boolean) : TSpkXMLNode;
       function GetSubNodeByIndex(index : integer) : TSpkXMLNode;

       function GetCount : integer;
     public
       constructor create; overload;
       constructor create(Aname : string); overload;
       destructor destroy; override;

       procedure Add(ANode : TSpkXMLNode);
       procedure Delete(index : integer);
       procedure Clear;

       property SubNodeByName[index : string; autocreate : boolean] : TSpkXMLNode read GetSubNodeByName; default;
       property SubNodeByIndex[index : integer] : TSpkXMLNode read GetSubNodeByIndex;

       property Parameters : TSpkXMLParameters read FParameters;
       property Name : string read FName write FName;
       property Text : string read FText write FText;
       property Count : integer read GetCount;
     published
     end;

type TSpkXMLParser = class(TObject)
     private
       FList : TList;
     protected
       function GetNodeByName(index : string; autocreate : boolean) : TSpkXMLNode;
       function GetNodeByIndex(index : integer) : TSpkXMLNode;
       function Build(node : TSpkXMLNode; indent : integer) : string;
       function ParseNode(s : string; var position : integer) : TSpkXMLNode;
     public
       constructor create;
       destructor destroy; override;

       procedure Add(ANode : TSpkXMLNode);
       procedure Delete(index : integer);
       procedure Clear;

       function XMLString : string;
       procedure SaveToFile(filename : string);
       procedure LoadFromFile(filename : string);
       procedure Parse(s : string);

       property NodeByName[index : string; autocreate : boolean] : TSpkXMLNode read GetNodeByName; default;
       property NodeByIndex[index : integer] : TSpkXMLNode read GetNodeByIndex;
     published
     end;

function Slash(s : string) : string;
function UnSlash(s : string) : string;

implementation

{ Utilities }

uses KSPConstsVars;

function IntToStr3(i : integer) : string;

begin
result:=inttostr(i);
while length(result)<3 do result:='0'+result;
end;

function Slash(s : string) : string;

begin
result:=s;
result:=StringReplace(result,'&','&amp;',[rfReplaceAll]);
result:=StringReplace(result,'<','&lt;',[rfReplaceAll]);
result:=StringReplace(result,'>','&gt;',[rfReplaceAll]);
result:=StringReplace(result,'''','&#039;',[rfReplaceAll]);
result:=StringReplace(result,'"','&quot;',[rfReplaceAll]);
end;

function Unslash(s : string) : string;

begin
result:=s;
result:=StringReplace(result,'&#039;','''',[rfReplaceAll]);
result:=StringReplace(result,'&quot;','"',[rfReplaceAll]);
result:=StringReplace(result,'&lt;','<',[rfReplaceAll]);
result:=StringReplace(result,'&gt;','>',[rfReplaceAll]);
result:=StringReplace(result,'&amp;','&',[rfReplaceAll]);
end;

{ TParseException }

constructor ParseException.create(AMessage : string; APosition : integer);

begin
self.Message:=AMessage;
FPosition:=APosition;
end;

{ TSpkXMLParameter }

constructor TSpkXMLParameter.create;

begin
inherited create;
FAttribute:='';
FValue:='';
end;

constructor TSpkXMLParameter.create(AAttribute, Avalue : string);

begin
inherited create;
FAttribute:=AAttribute;
FValue:=AValue;
end;

destructor TSpkXMLParameter.destroy;

begin
inherited destroy;
end;

{ TSpkXMLParameters }

function TSpkXMLParameters.GetParamByName(index : string; autocreate : boolean) : TSpkXMLParameter;

var i : integer;
    pom : TSpkXMLParameter;

begin
i:=0;
{$B-}
while (i<FList.count) and (uppercase(TSpkXMLParameter(FList[i]).Attribute)<>uppercase(index)) do inc(i);
if i<FList.count then result:=TSpkXMLParameter(FList[i]) else
   begin
   if not(autocreate) then result:=nil else
      begin
      pom:=TSpkXMLParameter.create(index,'');
      FList.add(pom);
      result:=pom;
      end;
   end;
end;

function TSpkXMLParameters.GetParamByIndex(index : integer) : TSpkXMLParameter;

begin
if (index<0) or (index>=FList.count) then
   raise exception.create(Format(sXMLIncorrectIndex, [IntToStr(index)]));//'TSpkXMLParameters.GetParamByIndex: Incorrect index ('+inttostr(index)+')');
result:=TSpkXMLParameter(FList[index]);
end;

function TSpkXMLParameters.GetCount : integer;

begin
result:=FList.count;
end;

constructor TSpkXMLParameters.create;

begin
inherited create;
FList:=TList.create;
end;

destructor TSpkXMLParameters.destroy;

begin
while FList.count>0 do Delete(0);
end;

procedure TSpkXMLParameters.Add(AParameter : TSpkXMLParameter);

begin
FList.add(AParameter);
end;

procedure TSpkXMLParameters.Add(Value, Attribute: string);
var
  AParameter: TSpkXMLParameter;
begin
  AParameter:=TSpkXMLParameter.create(Attribute, Value);
  Add(Aparameter);
end;

procedure TSpkXMLParameters.Delete(index : integer);

begin
if (index<0) or (index>=FList.count) then
   raise exception.create(Format(sXMLIncorrectIndex, [IntToStr(index)]));

TSpkXMLParameter(FList[index]).free;
FList.delete(index);
end;

procedure TSpkXMLParameters.Clear;

begin
while FList.count>0 do delete(0);
end;

{ TSpkXMLNode }

function TSpkXMLNode.GetSubNodeByName(index : string; autocreate : boolean) : TSpkXMLNode;

var i : integer;
    pom : TSpkXMLNode;

begin
i:=0;
{$B-}
while (i<FList.count) and (uppercase(TSpkXMLNode(FList[i]).name)<>uppercase(index)) do inc(i);
if i<FList.count then result:=TSpkXMLNode(FList[i]) else
   begin
   if not(autocreate) then result:=nil else
      begin
      pom:=TSpkXMLNode.create(index);
      FList.add(pom);
      result:=pom;
      end;
   end;
end;

function TSpkXMLNode.GetSubNodeByIndex(index : integer) : TSpkXMLNode;

begin
if (index<0) or (index>=FList.count) then
   raise exception.create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
result:=TSpkXMLNode(FList[index]);
end;

function TSpkXMLNode.GetCount : integer;

begin
result:=FList.count;
end;

constructor TSpkXMLNode.create;

begin
inherited create;
FParameters:=TSpkXMLParameters.create;
FList:=TList.create;
FName:='';
FText:='';
end;

constructor TSpkXMLNode.create(Aname : string);

begin
inherited create;
FParameters:=TSpkXMLParameters.create;
FList:=TList.create;
FName:=AName;
FText:='';
end;

destructor TSpkXMLNode.destroy;

begin
FParameters.free;
while FList.count>0 do delete(0);
inherited destroy;
end;

procedure TSpkXMLNode.Add(ANode : TSpkXMLNode);

begin
FList.add(ANode);
end;

procedure TSpkXMLNode.Delete(index : integer);

begin
if (index<0) or (index>=FList.count) then
   raise exception.create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
TSpkXMLNode(FList[index]).free;
FList.delete(index);
end;

procedure TSpkXMLNode.Clear;

begin
while FList.count>0 do delete(0);
end;

{ TSpkXMLParser }

function TSpkXMLParser.GetNodeByName(index : string; autocreate : boolean) : TSpkXMLNode;

var i : integer;
    pom : TSpkXMLNode;

begin
i:=0;
{$B-}
while (i<FList.count) and (uppercase(TSpkXMLNode(FList[i]).name)<>uppercase(index)) do inc(i);
if i<FList.count then result:=TSpkXMLNode(FList[i]) else
   begin
   if not(autocreate) then result:=nil else
      begin
      pom:=TSpkXMLNode.create(index);
      FList.add(pom);
      result:=pom;
      end;
   end;
end;

function TSpkXMLParser.GetNodeByIndex(index : integer) : TSpkXMLNode;

begin
if (index<0) or (index>=FList.count) then
   raise exception.create(Format(sXMLIncorrectIndex, [IntToStr(index)]));
result:=TSpkXMLNode(FList[index]);
end;

function TSpkXMLParser.Build(node : TSpkXMLNode; indent : integer) : string;

var i : integer;

  function MkIndent(i : integer) : string;

  begin
  result:='';
  while length(result)<i do result:=' '+result;
  end;

begin
result:='';
if (node.Text='') and (node.count=0) then
   begin
   result:=result+MkIndent(indent)+'<'+node.Name;
   if node.Parameters.count>0 then
      for i:=0 to node.parameters.count-1 do
          begin
          result:=result+' '+node.parameters.ParamByIndex[i].Attribute;
          result:=result+'="'+Slash(node.parameters.ParamByIndex[i].Value)+'"';
          end;
   result:=result+'/>'+LINE_BREAK;
   end
else
   begin
   result:=result+MkIndent(indent)+'<'+node.name;

   // Parametry

   if node.Parameters.count>0 then
      for i:=0 to node.parameters.count-1 do
          begin
          result:=result+' '+node.parameters.ParamByIndex[i].Attribute;
          result:=result+'="'+Slash(node.parameters.ParamByIndex[i].Value)+'"';
          end;
   result:=result+'>';

   // Tekst

   if node.Text<>'' then
      result:=result+Slash(node.text)+LINE_BREAK
   else
      result:=result+LINE_BREAK;

   // Podgałęzie

   if node.count>0 then
      for i:=0 to node.count-1 do
          result:=result+Build(node.SubNodeByIndex[i],indent+2);

   // Domknięcie

   result:=result+MkIndent(indent)+'</'+node.name+'>'+LINE_BREAK;
   end;
end;

function TSpkXMLParser.ParseNode(s : string; var position : integer) : TSpkXMLNode;

var s1,s2 : string;
    i : integer;
    node, subnode : TSpkXMLNode;
    len : integer;
    Quote : char;
    b : boolean;

begin
{$B-}
node:=nil;
len:=length(s);
try
// Znak "<"

  // Oczekiwany znak "<"
  if s[position]<>'<' then
     raise ParseException.create(sXMLIncorrectCharLower, position);//'Oczekiwany znak "<"',position);
  inc(position);

  // Plik nie może się tu zakończyć.
  if position>len then
     raise ParseException.create(sXMLIncorrectEOF, position);//'Nieoczekiwany koniec pliku - oczekiwano definicji znacznika',position);

  // Oczekiwany znacznik otwierający - nie może wystąpić znak '/'
  if s[position]='/' then
     raise ParseException.create(sXMLUnexpectedCloseTag, position);//'Nieoczekiwany znacznik zamykający',position);

// Nazwa gałęzi
  s1:='';
  while (position<=len) and (not(s[position] in [#9,' ','>','/'])) and (s1<>'!--') do
        begin
        if s[position] in [#0..#31] then
           raise ParseException.create(sXMLIncorrectTagName, position);//'Niewłaściwy znak w nazwie znacznika',position);
        s1:=s1+s[position];
        inc(position);
        end;

  // Dwa przypadki - komentarz lub nie
  if s1='!--' then
     begin
     // Szukanie końca komentarza
     while (position<=len) and (copy(s,position,3)<>'-->') do inc(position);
     if position>len then
        raise ParseException.create(sXMLNoEOC, position);//'Nieoczekiwany koniec pliku - oczekiwano końca komentarza',position);
     inc(position,3);
     end
  else
     begin
     node:=TSpkXMLNode.create(s1);
     // Ewentualne parametry
       repeat
       // Dowolna ilość spacji lub tabulacji
         while (position<=len) and (s[position] in [' ',#9]) do inc(position);
         if position>len then
            raise ParseException.create(sXMLIncorrectEOFParams, position);//'Nieoczekiwany koniec pliku - oczekiwano parametrów lub zakończenia znacznika',position);

       if not(s[position] in ['/','>']) then
          begin
          // Nazwa parametru, po niej dowolna ilość spacji lub tabulacji
            s1:='';
            while (position<=len) and (not(s[position] in [#9,' ','='])) do
                  begin
                  if s[position] in [#0..#31,'/','>','''','"'] then
                     raise ParseException.create(('sXMLIncorrectCharParamName'), position);//'Nieprawidłowy znak w nazwie parametru',position);
                  s1:=s1+s[position];
                  inc(position);
                  end;
            while (position<=len) and (s[position] in [' ',#9]) do inc(position);

            if (position>len) then
               raise ParseException.create(('sXMLIncorrectEOFEqual'), position);//'Nieoczekiwany koniec pliku - oczekiwano znaku "="',position);

          // znak = i dowolna ilość spacji lub tabulacji
            if s[position]<>'=' then
               raise ParseException.create(('sXMLIncorrectCharEqual'), position);//'Nieprawidłowy znak - oczekiwano "="',position);
            inc(position);
            while (position<=len) and (s[position] in [' ',#9]) do inc(position);
            if (position>len) then
               raise ParseException.create(('sXMLIncorrectEOFB'), position);//'Nieoczekiwany koniec pliku - oczekiwano znaku " lub ''',position);

          // znak " lub '
            if not(s[position] in ['"','''']) then
               raise ParseException.create(('sXMLIncorrectCharB'), position);//'Nieprawidłowy znak - oczekiwano " lub ''',position);
            quote:=s[position];
            inc(position);

          // Treść parametru zakończona odpowiednim ogranicznikiem w zależności od DoubleQuote
            s2:='';
            while (position<=len) and (s[position]<>quote) do
                  begin
//                  if s[position] in [#0..#31] then
//                     raise ParseException.create('Nieprawidłowy znak',position);
                  s2:=s2+s[position];
                  inc(position);
                  end;
            if (position>len) then
               raise ParseException.create(('sXMLIncorrectEOFNoParam'), position);//'Nieoczekiwany koniec pliku - oczekiwano domknięcia parametru',position);
            node.Parameters.Add(TSpkXMLParameter.create(s1,Unslash(s2)));

            inc(position);
            if (position>len) then
               raise ParseException.create(('sXMLIncorrectEOF'), position);//'Nieoczekiwany koniec pliku - oczekiwano parametru lub domknięcia znacznika',position);
          end;
     until s[position] in ['/','>'];

     // Dwie możliwości - samozamykający się znacznik (/) lub koniec
     // znacznika otwierającego (>)
       if s[position]='/' then
          begin
          // Znak >
            inc(position);
            if (position>len) then
               raise ParseException.create(('sXMLIncorrectEOF'), position);//'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika ">"',position);
            if s[position]<>'>' then
               raise ParseException.create(('sXMLIncorrectChar'), position);//'Nieprawidłowy znak - oczekiwano domknięcia znacznika ">"',position);
            inc(position);
          end
       else
          begin
          // Nie może tu być końca pliku - aż do znacznika domykającego
            inc(position);
            if position>len then
               raise ParseException.create(('sXMLIncorrectEOF'), position);//'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika '+node.name,position);

          // No i teraz niewielka rzeźnia, czyli tekst i podznaczniki.
            s2:=''; // Przechowuje tekst znacznika
            s1:='';
            while (position<=len) and (s[position]<>'<') do
                  begin
                  if s[position] in [#10,#13] then
                     begin
                     s2:=s2+s1;
                     s1:=s[position];
                     inc(position);
                     if position<=len then
                        if s[position] in [#10,#13] then
                           begin
                           s1:=s1+s[position];
                           inc(position);
                           end;
                     end
                  else
                     begin
                     s1:=s1+s[position];
                     inc(position);
                     end;
                  end;
            // Jeśli s1 jest postaci <enter>[spacje], nie dodawaj go do zawartości tekstu
            b:=true; // Fragment wyrównujący
            if length(s1)>1 then
               for i:=1 to length(s1) do
                   begin
                   if (i=1) and not(s1[i] in [#10,#13]) then b:=false else
                      if (i=2) and not(s1[i] in [#10,#13,' ',#9]) then b:=false else
                         if (i>2) and not(s1[i] in [' ',#9]) then b:=false;
                   end;
            if not(b) then s2:=s2+s1;

            node.text:=Unslash(s2);

          // Oczekiwane podznaczniki lub znacznik zamykający - pomiędzy nimi
          // dowolna ilość znaków białych (spacja, enter, tabulator)
            b:=false;
            repeat
            if (position>len) or (position+1>len) then
               raise ParseException.create(('sXMLIncorrectNoClosingTag')+node.Name, position);//'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika '+node.name,position);
            if s[position+1]='/' then b:=true else
               begin
               // Parsuj podgałąź
                 subnode:=ParseNode(s,position);
                 if subnode<>nil then //komentarz
                    node.Add(subnode);

               // Usuń pozostałe znaki białe aż do wystąpienia kolejnego znaku "<"
               while (position<=len) and (s[position] in [' ',#9,#10,#13]) do inc(position);

               if (position>len) then
                  raise ParseException.create(('sXMLIncorrectEOF'), position);//'Nieoczekiwany koniec pliku - oczekiwano domknięcia znacznika '+node.name,position);

               if s[position]<>'<' then
                  raise ParseException.create(('sXMLIncorrectChar'), position);//'Nieprawidłowy znak - tekst znacznika musi znaleźć się przed podznacznikami',position);
               end
            until b;

          // Osiągnięto </ - sprawdzamy, czy zgadza się nazwa znacznika otwierającego i zamykającego
             inc(position,2);

             // Dopuszczam znaki białe pomiędzy </ i nazwą znacznika
             while (position<=len) and (s[position] in [' ',#9]) do inc(position);

             if (position>len) then
                raise ParseException.create(('sXMLIncorrectEOF'), position);//'Nieoczekiwany koniec pliku - oczekiwano nazwy domykanego znacznika (w domyśle: '+node.name+')',position);

             s1:='';
             while (position<=len) and not(s[position] in ['>',' ',#9]) do
                   begin
                   if s[position] in [#0..#31] then
                      raise ParseException.create(('sXMLIncorrectChar'), position);//'Niewłaściwy znak w nazwie znacznika domykającego!',position);
                   s1:=s1+s[position];
                   inc(position);
                   end;
             if uppercase(s1)<>uppercase(node.Name) then
                raise ParseException.create(('sXMLIncorrectChar'), position);//'Znacznik otwierający nie odpowiada znacznikowi domykającemu, odpowiednio: "'+node.name+'" i "'+s1+'"',position);

             while (position<=len) and (s[position] in [' ',#9]) do inc(position);

             if position>len then
                raise ParseException.create(('sXMLIncorrectEOF'), position);//'Nieoczekiwany koniec pliku - oczekiwano znaku ">"',position);
             if s[position]<>'>' then
                raise ParseException.create(('sXMLIncorrectChar'), position);//'Nieprawidłowy znak - oczekiwano ">"',position);

          // Zakończono parsowanie znacznika XML. Ustawiamy pozycję na znak zaraz
          // po sparsowanym znaczniku.
            inc(position);
          end;
     end;

except
if node<>nil then node.free;
raise;
end;

result:=node;
end;

constructor TSpkXMLParser.create;

begin
inherited create;
FList:=TList.create;
end;

destructor TSpkXMLParser.destroy;

begin
while FList.count>0 do delete(0);
FList.free;
inherited destroy;
end;

procedure TSpkXMLParser.Add(ANode : TSpkXMLNode);

begin
FList.add(ANode);
end;

function TSpkXMLParser.XMLString : string;

var i : integer;

begin
result:='';
if FList.count>0 then
   for i:=0 to FList.count-1 do
       result:=result+Build(TSpkXMLNode(FList[i]),0);
end;

procedure TSpkXMLParser.Delete(index : integer);

begin
if (index<0) or (index>=FList.count) then
   raise exception.create(Format(('sXMLIncorrectIndex'), [IntToStr(index)]));
TSpkXMLNode(FList[index]).free;
FList.delete(index);
end;

procedure TSpkXMLParser.Clear;

begin
while FList.count>0 do delete(0);
end;

procedure TSpkXMLParser.SaveToFile(filename : string);

var fs : TStringList;

begin
fs:=TStringList.create;//(filename,fmCreate or fmShareDenyWrite);
fs.Text:=self.XMLString;
fs.SaveToFile(filename);
fs.Free;
end;

procedure TSpkXMLParser.LoadFromFile(filename : string);

var s : string;
    fs : TFileStream;

begin
fs:=TFileStream.create(filename,fmOpenRead or fmShareDenyWrite);
setlength(s,fs.Size);
fs.Read(s[1],fs.size);
fs.free;
Clear;
Parse(s);
end;

procedure TSpkXMLParser.Parse(s : string);

var i : integer;
    node : TSpkXMLNode;
    len : integer;
    line,ch : integer;

  procedure CharToLineChar(s : string; Ach : integer; var line : integer; var ch : integer);

  begin
  line:=1;
  ch:=Ach;
  while ((ch>min(pos(#13,s),pos(#10,s))) and (min(pos(#13,s),pos(#10,s))>0)) do
        begin
        inc(line);
        ch:=ch-min(pos(#13,s),pos(#10,s));
        System.delete(s,1,min(pos(#13,s),pos(#10,s)));
        if s[1] in [#13,#10] then
           begin
           dec(ch);
           system.delete(s,1,1);
           end;
        end;
  end;

begin
{$B-}
try
  self.Clear;
  i:=1;
  len:=length(s);

  while i<=len do
        begin
        while (i<=len) and (s[i] in [' ',#9,#10,#13]) do inc(i);
        if (i<=len) then
           begin
           if (s[i]='<'){or(s[i]='?')} then
              begin
              node:=ParseNode(s,i);
              if node<>nil then
                 Add(node);
              end else
                  raise ParseException.create(('sXMLIncorrectCharLower'),i);
           end;
        end;

except
  on e : ParseException do
     begin
     CharToLineChar(s,e.position,line,ch);
     raise exception.create(Format(('sXMLError'), [IntToStr(Line),
      IntToStr(ch), e.Message]));
      //'Wystąpił błąd podczas parsowania, linia '+inttostr(line)+' znak '+inttostr(ch)+', komunikat:'+#13+e.message);
     end;
end;

end;

end.
