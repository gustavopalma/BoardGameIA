unit uLigue4;

{$mode objfpc}{$H+} {$R+}{$Q+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, Types, math, LCLType;


const MAX_LINHA = 5;
const MAX_COLUNA = 6;
const CASA_VAZIA = 0;
const TAMANHO_JANELA = 4;
const PECA_VERMELHA = 2;
const PECA_AMARELA = 1;

var
 cont : Integer;

type
  TEstado = (eMovimento, eAvaliar, eEsperar);
type
  TTabuleiro = Array[0..MAX_LINHA,0..MAX_COLUNA] of Integer;
  PWidthArray = ^TTabuleiro;
  TLinha = Array of Integer;

type TMinMax = record
   coluna : Integer;
   pontuacao: Integer;
end;
type PInteger = ^Integer;

type
TMethodPtr = procedure of object;

  function pegalinha(tabuleiro : TTabuleiro; linha: Integer) : TLinha;
  function pegaColuna(tabuleiro: TTabuleiro; coluna: Integer): TLinha;
  function contaValores(vetor : TLinha; valor : Integer) : Integer;
  function pegaDiagonal(tabuleiro: TTabuleiro; linha, coluna,tamanho : Integer) : TLinha;
  function pegaDiagonal2(tabuleiro: TTabuleiro; linha, coluna,tamanho : Integer) : TLinha;
  function linhaAbertaTabuleiro(tabuleiro: TTabuleiro; coluna:Integer) : Integer;
  function localValido(tabuleiro: TTabuleiro; coluna : Integer) : Boolean;
  function poePca(tabuleiro : TTabuleiro; linha, coluna, peca: Integer) : TTabuleiro;
  function copiaJanela(linha: TLinha; inicio, tamanho : Integer) : TLinha;
  function movimentoVencedor(tabuleiro: TTabuleiro; peca: Integer ): Boolean;
  function determinaLocalValidos(tabuleiro : TTabuleiro) :TLinha;
  function finaliza_jogo(tabuleiro : TTabuleiro): Boolean;
  function copiaTabuleiro(origem : TTabuleiro) :TTabuleiro;

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
    procedure Execute; override;
    function minimax(tabuleiro : TTabuleiro;profundidade, alpha, beta:Integer; MaxJogador : Boolean) : TMinMax;
    function determinaFimdeJogo: Boolean;
    function pontuaPosicao(tab: TTabuleiro; peca: Integer) : Integer;
    function avaliaJanela(janela : TLinha; peca : Integer) : Integer;
  protected

  public
    constructor Create;
    destructor Destroy; override;

    property run: boolean read FRun write FRun default true;
    property estado : TEstado read FEstado write FEstado default eEsperar;
    property tabuleiro : PWidthArray read FTabuleiro write FTabuleiro;
    property totalJogadasPC : PInteger read FtotalJogadasPC write FtotalJogadasPC;
    property atualizaTela: TMethodPtr read FAtualizaTela write FAtualizaTela;
    property reiniciaJogo: TMethodPtr read FreiniciaJogo write FreiniciaJogo;
  end;

  type
  { TfrmLigue4 }
  TfrmLigue4 = class(TForm)
    DrawGrid1: TDrawGrid;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblTempo: TLabel;
    lblNJogadasHumano: TLabel;
    lblNJogadasIA: TLabel;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure atualizaTabuleiroVisual;

    procedure reiniciaJogo;
    function localValido(Coluna, corPeca: Integer): Boolean;
    procedure Timer1Timer(Sender: TObject);

  private
    tabuleiro : TTabuleiro;
    Player2 : TPlayer2;
    totalJogadasHumano : Integer;
    totalJogadasPC : Integer;
    hora, minuto, segundo : Integer;
  public

  end;

var
  frmLigue4: TfrmLigue4;

implementation

function pegalinha(tabuleiro: TTabuleiro; linha: Integer): TLinha;
var
  j : Integer;
begin
  SetLength(Result, 0);
  SetLength(Result, MAX_COLUNA + 1);
  for j := 0 to MAX_COLUNA do
  begin
    Result[j] := tabuleiro[linha, j];
  end;
end;

function pegaColuna(tabuleiro: TTabuleiro; coluna: Integer): TLinha;
var
  i : Integer;
begin
  SetLength(Result, 0);
  SetLength(Result, MAX_LINHA + 1);
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
 SetLength(Result, 0);
 SetLength(Result, tamanho);
 for k := 0 to tamanho - 1 do
  begin
   Result[k] := tabuleiro[linha - k, coluna + k];
  end;
end;

function pegaDiagonal2(tabuleiro: TTabuleiro; linha, coluna, tamanho: Integer
  ): TLinha;
var
  k : Integer;
begin
 SetLength(Result, 0);
 SetLength(Result, tamanho);
 for k := 0 to tamanho - 1 do
  begin
   Result[k] := tabuleiro[linha - k, coluna - k];
  end;
end;

function linhaAbertaTabuleiro(tabuleiro: TTabuleiro; coluna: Integer): Integer;
var
  i : Integer;
begin
 Result := -1;
  for i := MAX_LINHA downto 0  do
   begin
    if tabuleiro[i,coluna] = 0 then
     begin
      Result := i;
      break;
     end;
   end;
  if Result < 0 then
   Result := -1;
end;

function localValido(tabuleiro: TTabuleiro; coluna: Integer
  ): Boolean;
begin
  Result :=  tabuleiro[0, coluna] = CASA_VAZIA;
end;

function poePca(tabuleiro: TTabuleiro; linha, coluna, peca: Integer
  ): TTabuleiro;
begin
  if tabuleiro[linha, coluna] = CASA_VAZIA then
    tabuleiro[linha, coluna] := peca;

  Result := tabuleiro ;
end;

function copiaJanela(linha: TLinha; inicio, tamanho: Integer): TLinha;
var i, j : Integer;
begin
  SetLength(Result, 0);
  SetLength(Result, tamanho);
  j := 0;
  for i := 0 to 3  do
   begin
    Result[j] := linha[inicio + i];
    inc(j)
   end;
end;


function movimentoVencedor(tabuleiro: TTabuleiro; peca: Integer): Boolean;
var
  i, j : Integer;
begin
 Result := False;

 for j := 0 to MAX_COLUNA - 3 do
  begin
   for i := 0 to MAX_LINHA do
    begin
     if (tabuleiro[i,j] = peca) and (tabuleiro[i,j + 1] = peca) and
        (tabuleiro[i,j + 2] = peca) and (tabuleiro[i,j + 3] = peca) then
         begin
          Result := True;
          exit
         end;
    end;
  end;

 for j := 0 to MAX_COLUNA do
  begin
   for i := 0 to MAX_LINHA - 3 do
    begin
     if (tabuleiro[i,j] = peca) and (tabuleiro[i + 1,j] = peca) and
        (tabuleiro[i + 2, j] = peca) and (tabuleiro[i + 3, j] = peca) then
         begin
          Result := True;
          exit
         end;
    end;
  end;

 for j := 0 to MAX_COLUNA - 3 do
  begin
   for i := 3 to MAX_LINHA do
    begin
     if (tabuleiro[i,j] = peca) and (tabuleiro[i - 1,j + 1] = peca) and
        (tabuleiro[i - 2,j + 2] = peca) and (tabuleiro[i - 3,j + 3] = peca) then
         begin
          Result := True;
          exit
         end;
    end;
  end;

  for j := 3 to MAX_COLUNA do
  begin
   for i := 3 to MAX_LINHA do
    begin
     if (tabuleiro[i,j] = peca) and (tabuleiro[i - 1,j - 1] = peca) and
        (tabuleiro[i - 2,j - 2] = peca) and (tabuleiro[i - 3,j -3] = peca) then
         begin
          Result := True;
          exit
         end;
    end;
  end;
end;


function finaliza_jogo(tabuleiro: TTabuleiro): Boolean;
var locaisValidos : TLinha;
begin
  locaisValidos := determinaLocalValidos(tabuleiro);
  Result := movimentoVencedor(tabuleiro, PECA_VERMELHA) or movimentoVencedor(tabuleiro, PECA_AMARELA) or (Length(locaisValidos) = 0);
end;

function copiaTabuleiro(origem: TTabuleiro): TTabuleiro;
var i,j : Integer;
begin
  for i := 0 to MAX_LINHA do
   begin
     for j := 0 to MAX_COLUNA do
      begin
         Result[i,j] := origem[i,j];
      end;
   end;
end;

{$R *.lfm}

{ TPlayer2 }

procedure TPlayer2.Execute;
var
  valor, linha : Integer;
  minMaxResult : TMinMax;
begin
  While Frun do
  begin
   case Festado of
    eAvaliar:
     begin
       minMaxResult := minimax(FTabuleiro^,5, -MaxInt, MaxInt, True);
       valor := minMaxResult.pontuacao;
       FEstado := eMovimento;
     end;
    eMovimento:
     begin
      if localValido(FTabuleiro^, minMaxResult.coluna) then
       begin
        linha :=linhaAbertaTabuleiro(FTabuleiro^, minMaxResult.coluna);
        if localValido(FTabuleiro^, minMaxResult.coluna) then
          FTabuleiro^ := poePca(FTabuleiro^,linha,  minMaxResult.coluna, PECA_AMARELA);
        Inc(FtotalJogadasPC^);
       end;
       Synchronize(atualizaTela);
       determinaFimdeJogo;
       FEstado:= eEsperar;
     end;
    eEsperar:
      begin
       Sleep(200);
      end;
  end;
  end;
end;

function TPlayer2.minimax(tabuleiro: TTabuleiro; profundidade, alpha,
  beta: Integer; MaxJogador: Boolean): TMinMax;
var
 locaisValidos : TLinha;
 value : Integer;
 coluna, linha: Integer;
 j : integer;
 tabCopia : TTabuleiro;
 novaPontuacao : Integer;
 finalizado : boolean;
begin
  Result.coluna:= 0;
  Result.pontuacao:= 0;
  locaisValidos := determinaLocalValidos(tabuleiro);
  finalizado := finaliza_jogo(tabuleiro);

  if (profundidade = 0) or (finalizado) then
    begin
     if finalizado then
       begin
         if movimentoVencedor(tabuleiro, PECA_AMARELA) then
           begin
            Result.coluna:=-1;
            Result.pontuacao:=10000000;
            Exit(Result);
           end else
          if movimentoVencedor(tabuleiro, PECA_VERMELHA) then
            begin
             Result.coluna:=-1;
             Result.pontuacao:=-10000000;
             Exit(Result);
            end
          else
           begin
             Result.coluna:=-1;
             Result.pontuacao:=0;
             Exit(Result);
           end;
         end else
         begin
          Result.pontuacao := pontuaPosicao(tabuleiro, PECA_AMARELA);
          exit(Result);
          end;
        end;

  if MaxJogador then
    begin
     value  := -MaxInt;
     Randomize;
     coluna := Random(High(locaisValidos));
     for j in locaisValidos do
      begin
       linha := linhaAbertaTabuleiro(tabuleiro,j);
       tabCopia := copiaTabuleiro(tabuleiro);
       tabCopia := poePca(tabCopia,linha, j, PECA_AMARELA);
       novaPontuacao:= minimax(tabCopia,profundidade -1, alpha,beta, False).pontuacao;
       if novaPontuacao > value then
         begin
            value := novaPontuacao;
            coluna:= j;
         end;
       alpha := Max(novaPontuacao, alpha);
       if alpha >= beta then
       begin
        break;
       end;
      end;
     Result.coluna:= coluna;
     Result.pontuacao := value;
     exit(Result);
    end
    else
     begin
       value := MaxInt;
       coluna := Random(High(locaisValidos));
       for j in locaisValidos do
        begin
         linha := linhaAbertaTabuleiro(tabuleiro, j);
         tabCopia := copiaTabuleiro(tabuleiro);
         tabCopia := poePca(tabCopia,linha,j,PECA_VERMELHA);
         novaPontuacao := minimax(tabCopia, profundidade -1, alpha, beta, True).pontuacao;
         if novaPontuacao < value then
           begin
            value := novaPontuacao;
            coluna:= j;
           end;
         beta := min(novaPontuacao, beta);
         if alpha >= beta then
         begin
          break;
         end;
        end;
        Result.coluna:= coluna;
        Result.pontuacao := round(value);
        exit(Result);
     end;
end;

function TPlayer2.determinaFimdeJogo: Boolean;
var
Resultado: Integer;
msg:String;
locaisValidos : TLinha;
begin
  locaisValidos := determinaLocalValidos(FTabuleiro^);
  msg := EmptyStr;
  if movimentoVencedor(FTabuleiro^, PECA_VERMELHA) then
    msg := 'Você Venceu, Deseja Jogar Novamente?'
  else if movimentoVencedor(FTabuleiro^, PECA_AMARELA) then
    msg := 'computador Venceu, Deseja Jogar Novamente?'
  else if Length(locaisValidos) = 0 then
    msg :='Empate, Deseja Jogar Novamente?';

  if msg = EmptyStr then
     Exit(False);

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


function determinaLocalValidos(tabuleiro: TTabuleiro): TLinha;
var
  j,i : Integer;
begin
  i := 0;
  SetLength(Result, i);
  for j := 0 to MAX_COLUNA do
   begin
    if tabuleiro[0, j] = CASA_VAZIA then
     begin
      SetLength(Result, i + 1);
      Result[i] := j;
      Inc(i);
     end;
   end;
end;

function TPlayer2.pontuaPosicao(tab: TTabuleiro; peca: Integer): Integer;
var
 pontuacao, cont_peca : Integer;
 colunaDoMeio, colunaAux, linhaAux, janela : TLinha;
 i,j,k,l,m : Integer;
 linha : Array[0..6] of Integer;
 score : Array[0..1] of Integer;
begin
  pontuacao := 0;
  cont_peca := 0;

  colunaDoMeio := pegaColuna(tab, MAX_COLUNA div 2);
  cont_peca := contaValores(colunaDoMeio, peca);
  pontuacao:= cont_peca * 6;
  //Pontuacao na horizontal
   for i := 0 to MAX_LINHA do
   begin
    linhaAux := pegalinha(tab,i);
    for j:= 0 to MAX_COLUNA - 3 do
     begin
       janela := copiaJanela(linhaAux, j, TAMANHO_JANELA);
       pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;

  //Pontuacao na Vertical
  for j:= 0 to MAX_COLUNA do
   begin
    colunaAux := pegaColuna(tab, j);
    for i:= 0 to MAX_LINHA - 3 do
     begin
      janela := copiaJanela(colunaAux,i,TAMANHO_JANELA);
      pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;

  //Pontuacao na diagonal
  for i := 3 to MAX_LINHA do
   begin
    for j := 0 to MAX_COLUNA - 3 do
     begin
      janela := pegaDiagonal(tab, i, j, TAMANHO_JANELA);
      pontuacao := pontuacao + avaliaJanela(janela, peca);
     end;
   end;

  for i := 3 to MAX_LINHA do
   begin
    for j := 3 to MAX_COLUNA do
     begin
      janela := pegaDiagonal2(tab, i, j, TAMANHO_JANELA);
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
  pecaSW := PECA_AMARELA;

  if peca = PECA_AMARELA then
   pecaSW:= PECA_VERMELHA;

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
   Label1.Font.Color:= $0303cf;
   Label2.Font.Color:= $0303cf;
   lblNJogadasHumano.Font.Color:= $0303cf;

   Label3.Font.Color := $04E5FE;
   Label4.Font.Color := $04E5FE;
   lblNJogadasIA.Font.Color := $04E5FE;

   Player2 := TPlayer2.create();
   Player2.FreeOnTerminate:= True;
   Player2.estado := eEsperar;
   Player2.tabuleiro := @tabuleiro;
   Player2.atualizaTela:= @atualizaTabuleiroVisual;
   Player2.totalJogadasPC := @totalJogadasPC;
   Player2.reiniciaJogo := @reiniciaJogo;
   Player2.Start;

   hora := 0;
   minuto := 0;
   segundo := 0;
   cont := 0;
end;


procedure TfrmLigue4.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
if Player2.estado = eEsperar then
 begin
  PaintBox1.Canvas.Clear;
  PaintBox1.Canvas.Brush.Color:= TColor($0303cf);
  PaintBox1.Canvas.Ellipse(X - 45, Y - 45, X + 45, Y + 45);
  PaintBox1.Canvas.Brush.Color:= frmLigue4.Color;
 end
  else
    PaintBox1.Canvas.Clear;

end;


procedure TfrmLigue4.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 index : Integer;
begin
 if Player2.estado <> eEsperar then
  Exit;

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
  Inc(totalJogadasHumano);
  if localValido(index,PECA_VERMELHA) then
   begin
     atualizaTabuleiroVisual;
     if not Player2.determinaFimdeJogo then
          Player2.estado:= eAvaliar
   end;
end;

procedure TfrmLigue4.atualizaTabuleiroVisual;
begin
 DrawGrid1.BeginUpdate;
 DrawGrid1.EndUpdate;
 lblNJogadasHumano.Caption:= IntToStr(totalJogadasHumano);
 lblNJogadasIA.Caption:= IntToStr(totalJogadasPC);
end;

procedure TfrmLigue4.reiniciaJogo;
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
  lblNJogadasIa.Caption := IntToStr(totalJogadasPC);
  lblNJogadasHumano.Caption:=IntToStr(totalJogadasHumano);
  Timer1.Enabled := True;
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

procedure TfrmLigue4.Timer1Timer(Sender: TObject);
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





end.

