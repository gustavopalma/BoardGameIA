unit uJogoDaVelha;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Types, math, LCLType;

const MAX_LINHA = 2;
const MAX_COLUNA = 2;
const CASA_VAZIA = 0;
const X = 1;
const O = 2;

type
  TTabuleiro = Array[0..MAX_LINHA,0..MAX_COLUNA] of Integer;
  PWidthArray = ^TTabuleiro;
  TLinha = Array of Integer;

type
  TCoord = record
     X : Integer;
     Y : Integer;
  end;
type
  TCoords = Array of TCoord;

type
  TEstado = (eMovimento,eAvaliar,eEsperar);

type TMinMax = record
   ponto1 : Integer;
   ponto2 : Integer;
   pontuacao: Single;
end;

type
TMethodPtr = procedure of object;

  type
  { TPlayer2 }

  TPlayer2 = class(TThread)
  private
    FAtualizaTela: TMethodPtr;
    FEstado: TEstado;
    FreiniciaJogo: TMethodPtr;
    FRun: boolean;
    FTabuleiro: PWidthArray;
    FtotalJogadasPC: PInteger;
    Fvencedor: Integer;
    procedure Execute; override;
    function minimax(tabuleiro : TTabuleiro;profundidade, jogador: Integer) : TMinMax;
    function fimdejogo(tabuleiro : TTabuleiro) : Boolean;
    function determinaVencedor(tabuleiro : TTabuleiro; Jogador: Integer) : Boolean;
    function pontuaPosicao(tabuleiro: TTabuleiro) : Integer;
    function determinaLocalValidos(tabuleiro : TTabuleiro) :TCoords;
    procedure teste;
    {procedure determinaLocalValidos(tabuleiro : TTabuleiro);
    function avaliaJanela(janela : TLinha; peca : Integer) : Integer;}
  protected

  public
    constructor Create;
    destructor Destroy; override;

    property run: boolean read FRun write FRun default true;
    property estado : TEstado read FEstado write FEstado default eEsperar;
    property tabuleiro : PWidthArray read FTabuleiro write FTabuleiro;
    property atualizaTela: TMethodPtr read FAtualizaTela write FAtualizaTela;
    property vencedor : Integer read Fvencedor write Fvencedor;
    property totalJogadasPC : PInteger read FtotalJogadasPC write FtotalJogadasPC;
    property reiniciaJogo: TMethodPtr read FreiniciaJogo write FreiniciaJogo;
  end;


type

  { TfrmJogoDaVelha }

  TfrmJogoDaVelha = class(TForm)
    DrawGrid1: TDrawGrid;
    ImageList1: TImageList;
    lblJogadasIa: TLabel;
    lblTempo: TLabel;
    lblTempo1: TLabel;
    lblTempo2: TLabel;
    lblJogadasHumano: TLabel;
    lblTempo3: TLabel;
    Timer1: TTimer;
    procedure DrawGrid1Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure atualizaTabuleiroVisual;
    procedure Timer1Timer(Sender: TObject);
    procedure reiniciaJogo;
  private
   tabuleiro : TTabuleiro;
   Player2 : TPlayer2;
   totalJogadasHumano : Integer;
   totalJogadasPC : Integer;
   hora, minuto, segundo : Integer;
  public

  end;

var
  frmJogoDaVelha: TfrmJogoDaVelha;

implementation

{$R *.lfm}

{ TPlayer2 }

procedure TPlayer2.Execute;
var
  movimento : TMinMax;
  profundidade : Integer;
  locaisValidos : TCoords;
begin
 while Frun do
  begin
     case FEstado of
      eAvaliar:
       begin
        if fimdejogo(FTabuleiro^) then
         begin
          Synchronize(@teste);
          FEstado:= eEsperar;
          continue;
         end;
        locaisValidos := determinaLocalValidos(FTabuleiro^);
        profundidade := length(locaisValidos);
        movimento := minimax(FTabuleiro^,profundidade,O);
        FEstado := eMovimento;
       end;
      eMovimento:
       begin
         FTabuleiro^[movimento.ponto1, movimento.ponto2] := O;
         Inc(FtotalJogadasPC^);
         Synchronize(atualizaTela);
         if fimdejogo(FTabuleiro^) then
         begin
          Synchronize(@teste);
          FEstado:= eEsperar;
          continue;
         end;
        FEstado := eEsperar;
       end;
      eEsperar:
       begin
        Sleep(200);
       end;
     end;
  end;
end;

function TPlayer2.minimax(tabuleiro: TTabuleiro; profundidade, jogador: Integer
  ): TMinMax;
var
  pontuacao : TMinMax;
  locaisValidos : TCoords;
  localValido : TCoord;
  i : Integer;
begin
  Result.ponto1 := -1;
  Result.ponto2 := -1;
  if jogador = O then
   begin
     Result.pontuacao := -Infinity;
   end else
    begin
      Result.pontuacao := +Infinity;
    end;

    if (profundidade = 0) or (fimdejogo(tabuleiro)) then
     begin
        Result.ponto1 := -1;
        Result.ponto2 := -1;
        Result.pontuacao := pontuaPosicao(tabuleiro);
        exit;
     end;


    locaisValidos := determinaLocalValidos(tabuleiro);
    for i := 0 to Length(locaisValidos) - 1 do
     begin
       tabuleiro[locaisValidos[i].X, locaisValidos[i].Y] := jogador;
       if jogador = O then
        begin
         pontuacao := minimax(tabuleiro, profundidade - 1, X);
         tabuleiro[locaisValidos[i].X, locaisValidos[i].Y] := CASA_VAZIA;
         pontuacao.ponto1:= locaisValidos[i].X;
         pontuacao.ponto2:= locaisValidos[i].Y;
         if pontuacao.pontuacao > Result.pontuacao then
          Result := pontuacao;
        end
       else
        begin
          pontuacao := minimax(tabuleiro, profundidade - 1, O);
          tabuleiro[locaisValidos[i].X, locaisValidos[i].Y] := CASA_VAZIA;
          pontuacao.ponto1:= locaisValidos[i].X;
          pontuacao.ponto2:= locaisValidos[i].Y;
          if pontuacao.pontuacao < Result.pontuacao then
           Result := pontuacao;
        end;
     end;
end;

function TPlayer2.fimdejogo(tabuleiro: TTabuleiro): Boolean;
var
  lugaresValidos : Tcoords;
begin

   lugaresValidos := determinaLocalValidos(tabuleiro);

   if determinaVencedor(tabuleiro, X) then
    Fvencedor := X
   else if determinaVencedor(tabuleiro, O) then
    Fvencedor := O
   else
     Fvencedor := -1;
   Result := determinaVencedor(tabuleiro, X) or determinaVencedor(tabuleiro, O) or (Length(lugaresValidos) = 0)
end;

function TPlayer2.determinaVencedor(tabuleiro: TTabuleiro; Jogador: Integer
  ): Boolean;
var
  i, j : Integer;
begin
 Result := false;
 //verifica se o jogador ganhou numa linha
 for i := 0 to MAX_LINHA do
   begin
     if (tabuleiro[i,0] = Jogador) and (tabuleiro[i,1] = Jogador) and
        (tabuleiro[i,2] = Jogador) then
       begin
         Result := True;
         exit
       end;
   end;

 //verifica se o jogador ganhou numa coluna
  for j := 0 to MAX_COLUNA do
   begin
     if (tabuleiro[0,j] = Jogador) and (tabuleiro[1,j] = Jogador) and
        (tabuleiro[2,j] = Jogador) then
       begin
         Result := True;
         exit
       end;
   end;

  //verifica se o jogador ganhou numa diagonal
  if (tabuleiro[0,0] = Jogador) and (tabuleiro[1,1] = Jogador) and
     (tabuleiro[2,2] = Jogador) then
      begin
       Result := True;
       exit
      end;
  if (tabuleiro[0,2] = Jogador) and (tabuleiro[1,1] = Jogador) and
      (tabuleiro[2,0] = Jogador) then
      begin
       Result := True;
       exit
      end;
end;

function TPlayer2.pontuaPosicao(tabuleiro: TTabuleiro): Integer;
begin
  Result := 0;
  if determinaVencedor(tabuleiro, O) then
    Result := 1
  else if determinaVencedor(tabuleiro, X) then
    Result := -1
  else
   Result := 0;
end;

function TPlayer2.determinaLocalValidos(tabuleiro: TTabuleiro): TCoords;
var
  i, j, k, tamanho : Integer;
  aux : TCoord;
begin
  k := 0;
  tamanho := 0;
  SetLength(Result, tamanho);

  for i := 0 to MAX_LINHA do
   begin
     for j := 0 to MAX_COLUNA do
      begin
         if tabuleiro[i, j] = CASA_VAZIA then
           begin
            Inc(tamanho);
            SetLength(Result,tamanho);
            aux.X := i;
            aux.Y := j;
            Result[k] := aux;
            inc(k);
           end;
      end;
   end;
end;

procedure TPlayer2.teste;
var
Resultado: Integer;
msg:String;
begin
  if vencedor = X then
    msg := 'Você Venceu, Deseja Jogar Novamente?'
  else if vencedor = O then
    msg := 'computador Venceu, Deseja Jogar Novamente?'
  else
    msg :='Empate, Deseja Jogar Novamente?';

  Resultado := Application.MessageBox(Pchar(msg),'Fim de Jogo', MB_YESNO + MB_ICONEXCLAMATION);
  case Resultado of
    mryes: // usuário clicou Sim
     begin
       reiniciaJogo;
     end;
    mrNo:
     begin
      Application.Terminate;
     end;
  end;

end;

constructor TPlayer2.Create;
begin
  inherited Create(True);
  Frun := True;
end;

destructor TPlayer2.Destroy;
begin
  inherited Destroy;
end;

{ TfrmJogoDaVelha }

procedure TfrmJogoDaVelha.FormCreate(Sender: TObject);
var
  i, j : Integer;
begin
  for i := 0 to DrawGrid1.RowCount - 1 do
    begin
      DrawGrid1.RowHeights[i] := 200;
    end;

  for i := 0 to DrawGrid1.ColCount - 1 do
    begin
      DrawGrid1.ColWidths[i] := 200;
    end;

  for i := 0 to  MAX_LINHA do
    for j := 0 to MAX_COLUNA do
      begin
         tabuleiro[i,j] := CASA_VAZIA;
      end;

   totalJogadasHumano := 0;
   totalJogadasPC := 0;

   Player2 := TPlayer2.create();
   Player2.FreeOnTerminate:= True;
   Player2.estado := eEsperar;
   Player2.tabuleiro := @tabuleiro;
   Player2.atualizaTela := @atualizaTabuleiroVisual;
   Player2.totalJogadasPC := @totalJogadasPC;
   Player2.reiniciaJogo := @reiniciaJogo;
   Player2.Start;

end;

procedure TfrmJogoDaVelha.atualizaTabuleiroVisual;
begin
  DrawGrid1.BeginUpdate;
  DrawGrid1.EndUpdate;
  lblJogadasHumano.Caption:= IntToStr(totalJogadasHumano);
  lblJogadasIa.Caption:= IntToStr(totalJogadasPC);
end;

procedure TfrmJogoDaVelha.Timer1Timer(Sender: TObject);
begin
   Inc(segundo);
  if(segundo = 60) then
   begin
    segundo:= 0;
    Inc(Minuto);
   end;

  if(minuto = 60) then
   begin
    minuto:= 0;
    Inc(hora);
   end;

   lblTempo.Caption := Format('%2.2d',[Hora]) + ':' + Format('%2.2d',[Minuto]) + ':' + Format('%2.2d',[Segundo]);
end;

procedure TfrmJogoDaVelha.reiniciaJogo;
var
  i, j : Integer;
begin
 for i := 0 to MAX_LINHA do
  begin
   for j := 0 to MAX_COLUNA do
    begin
     tabuleiro[i,j] := CASA_VAZIA;
    end;
  end;
 atualizaTabuleiroVisual;
 Timer1.Enabled := False;
 segundo := 0;
 minuto := 0;
 hora := 0;
 lblTempo.Caption := Format('%2.2d',[Hora]) + ':' + Format('%2.2d',[Minuto]) + ':' + Format('%2.2d',[Segundo]);
 totalJogadasHumano := 0;
 totalJogadasPC := 0;
 lblJogadasIa.Caption := IntToStr(totalJogadasPC);
 lblJogadasHumano.Caption:=IntToStr(totalJogadasHumano);
 Timer1.Enabled := True;
end;

procedure TfrmJogoDaVelha.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  DrawGrid1.Canvas.Brush.Color := clBackground;
  DrawGrid1.Canvas.FillRect(aRect);
  ImageList1.Draw(DrawGrid1.Canvas,aRect.Left,aRect.Top,tabuleiro[aRow,aCol], True);
end;

procedure TfrmJogoDaVelha.DrawGrid1Click(Sender: TObject);
begin
  if (tabuleiro[DrawGrid1.Row, DrawGrid1.Col] = CASA_VAZIA) and
   (Player2.estado = eEsperar) then
   begin
    tabuleiro[DrawGrid1.Row, DrawGrid1.Col] := X;
    Inc(totalJogadasHumano);
    atualizaTabuleiroVisual;
    Player2.estado := eAvaliar;
   end;
end;

end.

