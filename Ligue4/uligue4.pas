unit uLigue4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Types,
  math;


const MAX_LINHA = 5;
const MAX_COLUNA = 6;
const CASA_VAZIA = 0;
const TAMANHO_JANELA = 4;
const PECA_VERMELHA = 2;
const PECA_AMARELA = 1;


type
  TEstado = (eMovimento,eAvaliar,eEsperar);
type
  TTabuleiro = Array[0..MAX_LINHA,0..MAX_COLUNA] of Integer;
  PWidthArray = ^TTabuleiro;
  TLinha = Array of Integer;


  function pegalinha(tabuleiro : TTabuleiro; linha: Integer) : TLinha;
  function pegaColuna(tabuleiro: TTabuleiro; coluna: Integer): TLinha;
  function contaValores(vetor : TLinha; valor : Integer) : Integer;
  function pegaDiagonal(tabuleiro: TTabuleiro; linha, coluna,tamanho : Integer) : TLinha;
  function linhaAbertaTabuleiro(tabuleiro: TTabuleiro; coluna:Integer) : Integer;
  function localValido(tabuleiro: TTabuleiro; coluna : Integer) : Boolean;
  procedure poePca(tabuleiro : TTabuleiro; linha, coluna, peca: Integer);

  type
  { TPlayer2 }

  TPlayer2 = class(TThread)
  private
    FEstado: TEstado;
    FRun: boolean;
    FTabuleiro: PWidthArray;
    locaisValidos: TStringArray;
    procedure Execute; override;
    function minimax(tabuleiro : TTabuleiro;profundidade, alpha, beta:Single; MaxJogador : Boolean; out colunaRet : Integer) : Integer;
    procedure teste;
    procedure determinaLocalValidos(tabuleiro : TTabuleiro);
    function pontuaPosicao(tabuleiro: TTabuleiro; peca: Integer) : Integer;
    function avaliaJanela(janela : TLinha; peca : Integer) : Integer;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    property run: boolean read FRun write FRun default true;
    property estado : TEstado read FEstado write FEstado default eEsperar;
    property tabuleiro : PWidthArray read FTabuleiro write FTabuleiro;
  end;

  type
  { TfrmLigue4 }
  TfrmLigue4 = class(TForm)
    DrawGrid1: TDrawGrid;
    ImageList1: TImageList;
    PaintBox1: TPaintBox;
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure atualizaTabuleiroVisual;


    function localValido(Coluna, corPeca: Integer): Boolean;

  private
    tabuleiro : TTabuleiro;
    Player2 : TPlayer2;

  public

  end;

var
  frmLigue4: TfrmLigue4;

implementation

function pegalinha(tabuleiro: TTabuleiro; linha: Integer): TLinha;
var
  i : Integer;
begin
  SetLength(Result, MAX_COLUNA);
  for i := 0 to MAX_COLUNA do
  begin
    Result[i] := tabuleiro[linha, i];
  end;
end;

function pegaColuna(tabuleiro: TTabuleiro; coluna: Integer): TLinha;
var
  i : Integer;
begin
  SetLength(Result, MAX_LINHA);
  for i := 0 to MAX_LINHA do
  begin
    Result[i] := tabuleiro[i, coluna];
  end;
end;

function contaValores(vetor: TLinha; valor: Integer): Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to High(vetor) do
   begin
     if vetor[i] = valor then
      begin
        inc(Result);
      end;
   end;
end;

function pegaDiagonal(tabuleiro: TTabuleiro; linha, coluna, tamanho: Integer
  ): TLinha;
var
  k : Integer;
begin
 for k := 0 to tamanho do
  begin
   Result[k] := tabuleiro[linha + k, coluna + k];
  end;
end;

function linhaAbertaTabuleiro(tabuleiro: TTabuleiro; coluna: Integer): Integer;
var
  i : Integer;
begin
  for i := 0 to MAX_LINHA do
   begin
    if tabuleiro[i,coluna] = 0 then
     begin
      Result := 0;
      break;
     end;
   end;
end;

function localValido(tabuleiro: TTabuleiro; coluna: Integer
  ): Boolean;
begin
  Result :=  tabuleiro[MAX_LINHA - 1, coluna] = CASA_VAZIA;
end;

procedure poePca(tabuleiro: TTabuleiro; linha, coluna, peca: Integer);
begin
  tabuleiro[linha, coluna] := peca;
end;


{$R *.lfm}

{ TPlayer2 }

procedure TPlayer2.Execute;
var
  colunaRet, valor, linha : Integer;

begin
  While Frun do
  begin

   case Festado of
    eMovimento:
     begin
      valor := minimax(FTabuleiro^,5, -Infinity, Infinity, True, colunaRet);
      if localValido(FTabuleiro^, colunaRet) then
       begin
        linha :=linhaAbertaTabuleiro(FTabuleiro^,colunaRet);
        poePca(FTabuleiro^,linha, colunaRet, PECA_AMARELA);
        FEstado:= eEsperar;
       end;
     end;
    eEsperar:
      begin
       Sleep(200);
       if tabuleiro^[0,0]= LongInt(2) then
         begin
//            Synchronize(@teste);
         end;
      end;
   end;
  end;
end;

function TPlayer2.minimax(tabuleiro: TTabuleiro; profundidade, alpha,
  beta: Single; MaxJogador: Boolean; out colunaRet: Integer): Integer;
var
  val : Single;
  aux,tmp, coluna, linha, col, novaPontuacao : Integer;
  x : String;
  i : Integer;
  tabCopia : TTabuleiro;
begin
  if profundidade = 0 then
   begin
     Result := pontuaPosicao(tabuleiro, PECA_AMARELA);
     exit;
   end;
  determinaLocalValidos(tabuleiro);
  if MaxJogador then
   begin
     val := -Infinity;
     Randomize;
     aux := Random(Length(locaisValidos) - 1);
     x := locaisValidos[aux].split(',')[1];
     coluna := strToInt(x);
     for i:= 0 to Length(LocaisValidos) - 1 do
      begin
         col := StrToInt(locaisValidos[i].trim.Split(',')[0]);
         linha := linhaAbertaTabuleiro(FTabuleiro^, col);
         tabCopia := tabuleiro;
         poePca(tabCopia,linha, col, PECA_AMARELA);
         novaPontuacao:= minimax(tabCopia, profundidade - 1, alpha, beta, False, colunaRet);
         if novaPontuacao > val then
          begin
            val := novaPontuacao;
            coluna := col;
          end;
         alpha := max(Alpha, val);
         if alpha >= beta then
          break;
      end;
      Result := round(val);
      colunaRet:= coluna;
   end
  else
   begin
    val := Infinity;
    Randomize;
    aux := Random(Length(locaisValidos) - 1);
    x := locaisValidos[aux].split(',')[1];
    coluna := strToInt(x);
    for i:= 0 to Length(LocaisValidos) - 1 do
     begin
      col := StrToInt(locaisValidos[i].trim.Split(',')[0]);
      linha := linhaAbertaTabuleiro(FTabuleiro^, col);
      tabCopia := tabuleiro;
      poePca(tabCopia,linha, col, PECA_VERMELHA);
      novaPontuacao:= minimax(tabCopia,profundidade - 1, alpha, beta, True, colunaRet);
      if novaPontuacao < val then
       begin
        val := novaPontuacao;
        coluna := col;
       end;
      beta := min(Beta, val);
      if beta >= alpha then
       break;
     end;
      Result := round(val);
      colunaRet:= coluna;
   end;
end;

procedure TPlayer2.teste;
begin
  showmessage('é o que vc esperava');
end;

procedure TPlayer2.determinaLocalValidos(tabuleiro: TTabuleiro);
var
  aux: String;
  i, j : Integer;
begin
 aux := '';
 for i := 0 to  MAX_LINHA do
  for j := 0 to MAX_COLUNA do
   begin
    if tabuleiro[i,j] = CASA_VAZIA then
     begin
      if aux = EmptyStr then
       aux := intToStr(i) + ',' + intToStr(j)
      else
       aux :=  aux + ';' + intToStr(i) + ',' + intToStr(j);
      end;
    end;

  if locaisValidos = nil then
  locaisValidos := TStringArray.Create;

 locaisValidos := aux.Split(';');


end;

function TPlayer2.pontuaPosicao(tabuleiro: TTabuleiro; peca: Integer): Integer;
var
 pontuacao, cont_peca : Integer;
 colunaDoMeio, colunaAux, linhaAux, janela : TLinha;
 i,j : Integer;

begin
 pontuacao := 0;
 cont_peca := 0;

  colunaDoMeio := pegaColuna(tabuleiro, MAX_COLUNA div 2);
  pontuacao:= pontuacao + cont_peca * 3;

  //Pontuacao na horizontal
  for i := 0 to MAX_LINHA do
   begin
    linhaAux := pegalinha(tabuleiro,i);
    for j:= 0 to MAX_COLUNA - 3 do
     begin
       janela := Copy(linhaAux,j, j + TAMANHO_JANELA);
       pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;

  //Pontuacao na Vertical
  for j:= 0 to MAX_COLUNA do
   begin
    colunaAux := pegaColuna(tabuleiro, j);
    for i:= 0 to MAX_LINHA - 3 do
     begin
      janela := Copy(colunaAux,i, i + TAMANHO_JANELA);
      pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;

  //Pontuacao na diagonal
  for i := 0 to MAX_LINHA - 3 do
   begin
    for j := 0 to MAX_COLUNA - 3 do
     begin
      janela := pegaDiagonal(tabuleiro, i, j, TAMANHO_JANELA);
      pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;

  for i := 0 to MAX_LINHA - 3 do
   begin
    for j := 0 to MAX_COLUNA - 3 do
     begin
      janela := pegaDiagonal(tabuleiro, i + 3, j, TAMANHO_JANELA);
      pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;
  Result := pontuacao;
end;

function TPlayer2.avaliaJanela(janela: TLinha; peca: Integer): Integer;
var
 pecaSW : Integer;
begin
  Result := 0;
  pecaSW := PECA_VERMELHA;

  if peca = PECA_VERMELHA then
   pecaSW:= PECA_AMARELA;

   if contaValores(janela, peca) = 4 then
    Inc(Result,100)
   else if (contaValores(janela, peca) = 3) and (contaValores(janela, CASA_VAZIA) = 1) then
    Inc(Result, 5)
   else if (contaValores(janela, peca) = 2) and (contaValores(janela, CASA_VAZIA) = 2) then
    Inc(Result, 2);

   if (contaValores(janela, pecaSW) = 3) and (contaValores(janela, CASA_VAZIA) = 1) then
    Dec(Result,4);
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

{ TfrmLigue4 }

procedure TfrmLigue4.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  DrawGrid1.Canvas.Brush.Color := clBackground;
  DrawGrid1.Canvas.FillRect(aRect);
  ImageList1.Draw(DrawGrid1.Canvas,aRect.Left,aRect.Top,tabuleiro[aRow,aCol], True);
end;

procedure TfrmLigue4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Player2.run:= False;
  Player2.WaitFor;
end;

procedure TfrmLigue4.FormCreate(Sender: TObject);
var
  i, j : Integer;
begin
  for i := 0 to DrawGrid1.RowCount - 1 do
    begin
      DrawGrid1.RowHeights[i] := 100;
    end;

  for i := 0 to DrawGrid1.ColCount - 1 do
    begin
      DrawGrid1.ColWidths[i] := 100;
    end;

  for i := 0 to  MAX_LINHA do
    for j := 0 to MAX_COLUNA do
      begin
         tabuleiro[i,j] := CASA_VAZIA;
      end;

   Player2 := TPlayer2.create();
   Player2.FreeOnTerminate:= True;
   Player2.estado := eEsperar;
   Player2.tabuleiro := @tabuleiro;
   Player2.Start;
end;

procedure TfrmLigue4.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 PaintBox1.Canvas.Clear;
 PaintBox1.Canvas.Brush.Color:= TColor($0303cf);
 PaintBox1.Canvas.Ellipse(X - 45, Y - 45, X + 45, Y + 45);
 PaintBox1.Canvas.Brush.Color:= frmLigue4.Color;
end;

procedure TfrmLigue4.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 index : Integer;
begin
  if X < 100 then
   begin
    index  := 0;
   end
  else if X < 200 then
   begin
    index  := 1;
   end
  else if X < 300 then
   begin
    index  := 2;
   end
  else if X < 400 then
   begin
    index  := 3;
   end
  else if X < 500 then
   begin
    index  := 4;
   end
  else if X < 600 then
   begin
    index  := 5;
   end
  else
   begin
    index  := 6;
   end;

  if localValido(index,PECA_VERMELHA) then
   begin
     atualizaTabuleiroVisual;
     Player2.estado:= eMovimento;
   end;
end;

procedure TfrmLigue4.atualizaTabuleiroVisual;
begin
 DrawGrid1.BeginUpdate;
 DrawGrid1.EndUpdate;
end;

function TfrmLigue4.localValido(Coluna, corPeca: Integer): Boolean;
var i : Integer;
begin
  Result := False;
  for i := MAX_LINHA downto 0 do
    begin
      if tabuleiro[i, Coluna] = 0 then
        begin
           tabuleiro[i, Coluna] := corPeca;
           Result := True;
           break;
        end;
    end;
end;





end.

