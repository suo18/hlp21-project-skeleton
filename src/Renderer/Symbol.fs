module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//


/// Model to generate one symbol. Id is a unique Id 
/// for the symbol shared with Issie Component type.

type SymbolCategory = 
    | IO 
    | Buses  
    | Gates
    | Adder
    | FlipFlop
    | Multiplexer
    | Wire
    | Registers
    | Decoder
    | Constants 
    | CustomComponent
    | RandomAccess
    | ReadOnly
    | Unknown
    
type PortInfo = {
    Pos : XYPos
    PortNumber : int option
    PortType : CommonTypes.PortType
    HostId : CommonTypes.ComponentId 
}

type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType
        BBox : BoundingBox // creates a bounding box for Sheet Module
        Label : string
        PortInfoList : PortInfo list

    }

type Model = Symbol list

//----------------------------Message Type. N.B. this is dissimilar to documentation because mouse messages are implemented by sheet for group 09-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    // | AddCircle of XYPos // used by demo code to add a circle, will remove
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
    | AddSymbol of sTyp : CommonTypes.ComponentType * pagepos: XYPos * label: string // tells you the type of component it is and its starting position


//---------------------------------helper types and functions----------------//



let posDiff (a: XYPos) (b: XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a: XYPos) (b: XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

//creates bounding boxes for each symbol category 
let createBBox (cat: SymbolCategory) (pos: XYPos) = 
    match cat with 
    | Gates -> {TopLeft = { pos with Y = pos.Y - 30. } ; BottomRight= { X = pos.X + 105.; Y = pos.Y + 95.}}
    // Gates -> {TopLeft = { pos with Y = pos.Y - 20. } ; BottomRight= { X = pos.X + 75.; Y = pos.Y + 95.}}
    | Adder | ReadOnly | CustomComponent -> {TopLeft = {X = pos.X - 5. ; Y = pos.Y + 30.} ; BottomRight = {X = pos.X + 95.; Y = pos.Y + 155.}}
    | FlipFlop -> {TopLeft = { X = pos.X - 5. ; Y = pos.Y - 30.} ; BottomRight= { X = pos.X + 80.; Y = pos.Y + 80.}}
    | IO -> {TopLeft = {X = pos.X - 5. ; Y = pos.Y - 30.} ; BottomRight = {X = pos.X + 75. ; Y = pos.Y + 55.}}
    | Wire -> {TopLeft = {X = pos.X - 5.; Y = pos.Y - 40.} ; BottomRight = { X = pos.X + 45.; Y = pos.Y + 20.}}
    | Multiplexer -> {TopLeft = {X = pos.X - 5.; Y = pos.Y - 30.} ; BottomRight = {X = pos.X + 35.; Y = pos.Y + 75.}}
    | Buses -> {TopLeft = {X = pos.X - 5.; Y = pos.Y - 30.} ; BottomRight = {X = pos.X + 80.; Y = pos.Y + 26.}}
    | Registers | RandomAccess -> {TopLeft = {X = pos.X - 5.; Y = pos.Y - 30.} ; BottomRight = {X = pos.X + 130.; Y = pos.Y + 105.}}
    | Decoder -> {TopLeft = {X = pos.X - 5. ; Y = pos.Y + 30.} ; BottomRight = {X = pos.X + 95.; Y = pos.Y + 155.}}
    | Constants -> {TopLeft = {X = pos.X - 5. ; Y = pos.Y + 30.} ; BottomRight = {X = pos.X + 55.; Y = pos.Y + 45.}}
    | _-> failwithf "not implemented"

//get portinfo for a specific symbol, returns a list of (PortType, PortNumber and Port Position) for all the ports
// let getportInfo (comp: CommonTypes.ComponentType) (pos :XYPos) = 
//     match comp with 
//     | And | Or | Xnor | Xor | Nor | Nand -> [("Input", 0, { pos with Y = pos.Y + 25.}); ("Input", 1; {pos with Y = pos.Y + 50.}); ("Output", 0, {pos with Y = pos.Y + 50.})]
//     |_-> []

//-----------------------------Abstraction functions----------------//


//groups symbnols into categories similar to implementation in Issie draw 2D
let getSymbolCategory (comp : CommonTypes.ComponentType) = 
    match comp with 
    | And | Or | Not | Nand | Nor | Xor | Xnor ->  Gates
    | NbitsAdder x -> Adder
    | DFF | DFFE -> FlipFlop
    | Input y | Output y -> IO
    | IOLabel -> IO
    | Mux2 | Demux2 -> Multiplexer
    | MergeWires -> Wire
    | SplitWire x -> Wire
    | Register x | RegisterE x -> Registers
    | BusSelection (x,y) -> Buses
    | Decode4 -> Decoder
    | Constant (x,y) -> Constants
    | Custom c -> CustomComponent
    | AsyncROM mem -> ReadOnly
    | ROM mem -> ReadOnly
    | RAM mem -> RandomAccess
    |_-> Unknown // never matched ifor individual demo, but used in case new symbol types are added

//for Busselection, returns the bus values to be printed 
let getBus (comp:CommonTypes.ComponentType) =
    match comp with 
    | BusSelection (x,y) -> ((x + y - 1) , y)
    |_-> failwithf "not implemented for this symbol"

//for each symbol, returns the character to be displayed 
let getGateDisplayChar (comp: CommonTypes.ComponentType) = 
    match comp with 
    | And -> "&"
    | Or -> "≥1"
    | Not -> "1"
    | Nand -> "&"
    | Nor -> "≥1"
    | Xor -> "=1"
    | Xnor -> "=1"
    | NbitsAdder x -> sprintf "%A" (x - 1) 
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | Register x -> sprintf "%A" (x) 
    | RegisterE x -> sprintf "%A" (x) 
    | Input x -> sprintf "%A" (x - 1) 
    | Output x -> sprintf "%A" (x - 1) 
    | Constant (x,y) -> sprintf "0x%0x" y
    | AsyncROM mem -> "Async-ROM"
    | ROM mem -> "ROM"
    | RAM mem -> "RAM"
    | Custom c -> c.Name
    |_-> ""

// returns true if logic gate is an inverter
let getIsInverter (comp: CommonTypes.ComponentType) = 
    match comp with 
    | And | Or | Xor -> false
    | Not | Nand | Nor | Xnor-> true
    |_-> false

//returns true if the symbol has an enable input 
let IsEnable (comp: CommonTypes.ComponentType) = 
    match comp with 
    | DFFE -> true
    | DFF -> false
    | Register x -> false
    | RegisterE x -> true
    | _-> false

let isInput (comp: CommonTypes.ComponentType) = 
    match comp with
    | Input a -> true
    | _-> false

let isLabel (comp : CommonTypes.ComponentType) = 
    match comp with
    | IOLabel -> true
    | _-> false

let IsSync (comp : CommonTypes.ComponentType) = 
    match comp with
    | ROM mem -> true
    | _-> false

//returns tru if symbol is a multiplexer
let isMux (comp: ComponentType) = 
    match comp with
    | Mux2 -> true
    | _-> false

let isMerge (comp: ComponentType) = 
    match comp with
    | MergeWires -> true
    | _-> false


let getInputList (typ : CommonTypes.ComponentType) =
    match typ with
    | Custom c -> c.InputLabels |> List.map (fun (s, i) -> s)
    | _-> []


let getOutputList (typ : CommonTypes.ComponentType) =
    match typ with
    | Custom c -> c.OutputLabels |> List.map (fun (s, i) -> s)
    | _-> []
//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
let createNewSymbol (input: XYPos * CommonTypes.ComponentType * string) =
    let (pos, compTyp, labl) = input
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this                          
        IsDragging = false // initial value can always be this
        Id = CommonTypes.ComponentId (Helpers.uuid()) 
        Type = compTyp
        BBox = createBBox (getSymbolCategory compTyp) (pos)
        Label = labl
        PortInfoList = []
    }

/// Dummy function for test. The real init would probably have no symbols.
/// init contains our initial symbols that in our instance should 
let init () = 
    [
        ({X = 300.; Y = 600.}, And, "G1");
        ({X = 300.; Y = 800.}, NbitsAdder 5, "A1");
        ({X = 200.; Y = 500.}, Input 5, "I1");
        ({X = 300.; Y = 500.}, Output 5, "01");
        ({X = 200.; Y = 200.}, IOLabel, "L1");
        ({X = 325.; Y = 300.}, Mux2, "MUX1");
        ({X = 250.; Y = 100.}, Demux2, "DM1");
        ({X = 75.; Y = 275.}, Register 8, "REG1");
        ({X = 50.; Y = 400.}, RegisterE 10, "REG2");
        ({X = 100.; Y = 100.}, MergeWires, "MW1");
        ({X = 100.; Y = 200.}, SplitWire 10, "MW2");
        ({X = 100.; Y = 650.}, BusSelection (10,5), "BS1");
        ({X = 100.; Y = 700.}, BusSelection (10,0), "BS2");
        ({X = 40.; Y = 550.}, Constant (2,50), "C2");
        ({X = 40.; Y = 625.}, Constant (2,30), "C3");
        ({X = 100.; Y = 800.}, Decode4, "D1");
        ({X = 300.; Y = 100.}, DFF, "DFF1");
        ({X = 500.; Y = 200.}, DFFE, "DFF2");
        ({X = 600.; Y = 200.}, AsyncROM {AddressWidth = 5; WordWidth = 10; Data = Map<int64,int64>[] }, "ROM1");
        ({X = 600.; Y = 400.}, ROM {AddressWidth = 5; WordWidth = 10; Data = Map<int64,int64>[] }, "ROM2");
        ({X = 450.; Y = 700.}, RAM {AddressWidth = 10; WordWidth = 20; Data = Map<int64,int64>[] }, "RAM1");
        ({X = 750.; Y = 200.}, Custom {Name = "custom";InputLabels = [("A",2); ("B",3); ("C",3)]; OutputLabels = [("D",2); ("E",3)]}, "CT");
        ({X = 470.; Y = 500.}, Or, "G2");
        ({X = 450.; Y = 600.}, Not, "G3");
        ({X = 470.; Y = 400.}, Nor, "G4");
        ({X = 650.; Y = 700.}, Xor, "G5");
        ({X = 650.; Y = 600.}, Xnor, "G6");
        ({X = 300.; Y = 700.}, Nand, "G7")
    ]
    |> List.map createNewSymbol
    , Cmd.none

/// update function which displays symbols -> will be handled by Sheet
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (sTyp,pos,label) -> 
        (createNewSymbol (pos,sTyp,label)):: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    | Dragging (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if rank <> sym.Id then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
                { sym with
                    Pos = posAdd sym.Pos diff
                    LastDragPos = pagePos
                }
        )
        , Cmd.none

    | EndDragging sId ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
/// all the symbols will have this type
type RenderSymbolProps =                
    {
        Rectangle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }


let renderGate = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 75.
                        SVGAttr.Height 75.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]

                text
                    [ X (props.Rectangle.Pos.X + 35.) 
                      Y (props.Rectangle.Pos.Y + 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "30px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str displaychar]
                text
                    [ X (props.Rectangle.Pos.X + 35.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]                   
                line [ X1 (props.Rectangle.Pos.X + 75.); Y1 (props.Rectangle.Pos.Y + 37.5); X2 (props.Rectangle.Pos.X + 95.); Y2 (props.Rectangle.Pos.Y + 37.5); Style[Stroke "Black"]] []
                if getIsInverter props.Rectangle.Type
                then line [ X1 (props.Rectangle.Pos.X + 75.); Y1 (props.Rectangle.Pos.Y + 25.); X2 (props.Rectangle.Pos.X + 85.); Y2 (props.Rectangle.Pos.Y + 37.5); Style[Stroke "Black"]] []
            ]
    )

let renderNbitsAdder = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 90.
                        SVGAttr.Height 125.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]

                text
                    [ X (props.Rectangle.Pos.X + 40.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 45.) 
                      Y (props.Rectangle.Pos.Y )
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "15px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "adder(" + displaychar + ":0)"]
                //left inputs    
                text
                    [ X (props.Rectangle.Pos.X + 10.) 
                      Y (props.Rectangle.Pos.Y + 26.25)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "Cin"]
                text
                    [ X (props.Rectangle.Pos.X + 5.) 
                      Y (props.Rectangle.Pos.Y  + 57.5)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "A"]
                text
                    [ X (props.Rectangle.Pos.X + 5.) 
                      Y (props.Rectangle.Pos.Y + 88.75)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "B"]
                //right outputs
                text
                    [ X (props.Rectangle.Pos.X + 75.) 
                      Y (props.Rectangle.Pos.Y + 38.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "Sum"] 
                text
                    [ X (props.Rectangle.Pos.X + 75.) 
                      Y (props.Rectangle.Pos.Y + 80.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "Cout"]                                                                                                  
            ]
    )

let renderDecoder = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 90.
                        SVGAttr.Height 120.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]

                text
                    [ X (props.Rectangle.Pos.X + 40.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 45.) 
                      Y (props.Rectangle.Pos.Y )
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "15px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "decode"]
                //left inputs    
                text
                    [ X (props.Rectangle.Pos.X + 10.) 
                      Y (props.Rectangle.Pos.Y + 40.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "Sel"]
                text
                    [ X (props.Rectangle.Pos.X + 12.) 
                      Y (props.Rectangle.Pos.Y  + 80.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "data"]
                //right outputs
                text
                    [ X (props.Rectangle.Pos.X + 75.) 
                      Y (props.Rectangle.Pos.Y + 24.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "0"] 
                text
                    [ X (props.Rectangle.Pos.X + 75.) 
                      Y (props.Rectangle.Pos.Y + 48.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "1"]    
                text
                    [ X (props.Rectangle.Pos.X + 75.) 
                      Y (props.Rectangle.Pos.Y + 72.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "2"]                                                                                                  
                text
                    [ X (props.Rectangle.Pos.X + 75.) 
                      Y (props.Rectangle.Pos.Y + 96.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "3"]       
            ]
    )

let renderROM = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 100.
                        SVGAttr.Height 110.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
                text
                    [ X (props.Rectangle.Pos.X + 50.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 50.) 
                      Y (props.Rectangle.Pos.Y + 1.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "15px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str displaychar]
                
                //left symbols
                text
                    [ X (props.Rectangle.Pos.X + 20.) 
                      Y (props.Rectangle.Pos.Y + 50.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "addr"]

                //right symbol
                text
                    [ X (props.Rectangle.Pos.X + 80.) 
                      Y (props.Rectangle.Pos.Y + 50.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "data"]
                // clock indentation with two intersecting lines
                if IsSync props.Rectangle.Type then
                    text
                        [ X (props.Rectangle.Pos.X + 18.) 
                          Y (props.Rectangle.Pos.Y + 80.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "12.5px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "clk"]
                    line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 80.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 85.); Style[Stroke "Black"]] []
                    line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 90.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 85.); Style[Stroke "Black"]] []
            ]
    )

let renderRAM = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 125.
                        SVGAttr.Height 100.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
                text
                    [ X (props.Rectangle.Pos.X + 60.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 60.) 
                      Y (props.Rectangle.Pos.Y + 1.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "15px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str displaychar]
                
                //left symbols
                text
                    [ X (props.Rectangle.Pos.X + 15.) 
                      Y (props.Rectangle.Pos.Y + 25.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "addr"]
                text
                    [ X (props.Rectangle.Pos.X + 20.) 
                      Y (props.Rectangle.Pos.Y + 50.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "data-in"]
                text
                    [ X (props.Rectangle.Pos.X + 15.) 
                      Y (props.Rectangle.Pos.Y + 75.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "write"]                                       

                //right symbol
                text
                    [ X (props.Rectangle.Pos.X + 110.) 
                      Y (props.Rectangle.Pos.Y + 50.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "data"]
                // clock indentation with two intersecting lines

                text
                    [ X (props.Rectangle.Pos.X + 18.) 
                      Y (props.Rectangle.Pos.Y + 90.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "clk"]
                line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 90.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 95.); Style[Stroke "Black"]] []
                line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 100.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 95.); Style[Stroke "Black"]] []
            ]
    )

let renderFlipFlop = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 75.
                        SVGAttr.Height 75.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
                text
                    [ X (props.Rectangle.Pos.X + 35.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 35.) 
                      Y (props.Rectangle.Pos.Y )
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "15px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str displaychar]
                
                //left symbols
                text
                    [ X (props.Rectangle.Pos.X + 8.) 
                      Y (props.Rectangle.Pos.Y + 25.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "D"]

                text
                    [ X (props.Rectangle.Pos.X + 18.) 
                      Y (props.Rectangle.Pos.Y + 49.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "clk"]
                //right symbol
                text
                    [ X (props.Rectangle.Pos.X + 65.) 
                      Y (props.Rectangle.Pos.Y + 37.5)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "Q"]
                //optional enable
                if IsEnable props.Rectangle.Type
                then
                    text
                        [ X (props.Rectangle.Pos.X + 37.5) 
                          Y (props.Rectangle.Pos.Y + 65.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "10px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "EN"]
                // clock indentation with two intersecting lines
                line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 50.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 55.); Style[Stroke "Black"]] []
                line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 60.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 55.); Style[Stroke "Black"]] []
            ]
    )

let renderIO = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                if isInput props.Rectangle.Type then
                    polygon
                        [ 
                            SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                        props.Rectangle.Pos.X props.Rectangle.Pos.Y (props.Rectangle.Pos.X + 50.)
                                                        props.Rectangle.Pos.Y (props.Rectangle.Pos.X + 70.) (props.Rectangle.Pos.Y + 25.) 
                                                        (props.Rectangle.Pos.X + 50.) (props.Rectangle.Pos.Y + 50.) props.Rectangle.Pos.X (props.Rectangle.Pos.Y + 50.))
                            X props.Rectangle.Pos.X
                            Y props.Rectangle.Pos.Y
                            SVGAttr.Fill color
                            SVGAttr.FillOpacity 0.75
                            SVGAttr.Stroke "Black"
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]
                    text
                        [ X (props.Rectangle.Pos.X + 35.) 
                          Y (props.Rectangle.Pos.Y - 20.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "20px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str <| props.Rectangle.Label + "(" + displaychar + ":0)"]                           

                else if isLabel props.Rectangle.Type then
                    polygon
                        [ 
                            SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                    (props.Rectangle.Pos.X + 20.) (props.Rectangle.Pos.Y + 50.)
                                                    (props.Rectangle.Pos.X ) (props.Rectangle.Pos.Y + 25.)
                                                    (props.Rectangle.Pos.X + 20.) (props.Rectangle.Pos.Y) 
                                                    (props.Rectangle.Pos.X + 50.) (props.Rectangle.Pos.Y) 
                                                    (props.Rectangle.Pos.X + 70.) (props.Rectangle.Pos.Y + 25.) 
                                                    (props.Rectangle.Pos.X + 50.) (props.Rectangle.Pos.Y + 50.)) 
                                                    
                            X props.Rectangle.Pos.X
                            Y props.Rectangle.Pos.Y
                            SVGAttr.Fill color
                            SVGAttr.FillOpacity 0.75
                            SVGAttr.Stroke "Black"
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]
                else 
                    polygon
                        [ 
                            SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                    (props.Rectangle.Pos.X + 20.) (props.Rectangle.Pos.Y + 50.)
                                                    (props.Rectangle.Pos.X ) (props.Rectangle.Pos.Y + 25.)
                                                    (props.Rectangle.Pos.X + 20.) (props.Rectangle.Pos.Y) 
                                                    (props.Rectangle.Pos.X + 70.) props.Rectangle.Pos.Y
                                                    (props.Rectangle.Pos.X + 70.) (props.Rectangle.Pos.Y + 50.))
                            X props.Rectangle.Pos.X
                            Y props.Rectangle.Pos.Y
                            SVGAttr.Fill color
                            SVGAttr.FillOpacity 0.75
                            SVGAttr.Stroke "Black"
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]

                    text
                        [ X (props.Rectangle.Pos.X + 35.) 
                          Y (props.Rectangle.Pos.Y - 20.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "20px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str <| props.Rectangle.Label + "(" + displaychar + ":0)"]   
                ]
    )

let renderConstant = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [

                polygon
                    [ 
                        SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                    props.Rectangle.Pos.X props.Rectangle.Pos.Y
                                                    (props.Rectangle.Pos.X + 20.) (props.Rectangle.Pos.Y + 20.)
                                                    (props.Rectangle.Pos.X ) (props.Rectangle.Pos.Y + 40.))
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]

                text
                    [ X (props.Rectangle.Pos.X + 35.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| props.Rectangle.Label] 

                line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y + 20.); X2 (props.Rectangle.Pos.X + 50.); Y2 (props.Rectangle.Pos.Y + 20.); Style[Stroke "Black"]] [] 

                text
                    [ X (props.Rectangle.Pos.X + 35.) 
                      Y (props.Rectangle.Pos.Y + 30.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| displaychar]   
                ]
    )

let renderMux = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [   if isMux props.Rectangle.Type then
                    polygon
                        [ 
                            SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                        props.Rectangle.Pos.X props.Rectangle.Pos.Y
                                                        (props.Rectangle.Pos.X + 30.)(props.Rectangle.Pos.Y + 25.)
                                                        (props.Rectangle.Pos.X + 30.) (props.Rectangle.Pos.Y + 45.) 
                                                        (props.Rectangle.Pos.X) (props.Rectangle.Pos.Y + 70.))
                            X props.Rectangle.Pos.X
                            Y props.Rectangle.Pos.Y
                            SVGAttr.Fill color
                            SVGAttr.FillOpacity 0.75
                            SVGAttr.Stroke "Black"
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]
                    
                    text
                        [ X (props.Rectangle.Pos.X + 5.) 
                          Y (props.Rectangle.Pos.Y + 23.3)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "8px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "0"]

                    text
                        [ X (props.Rectangle.Pos.X + 5.) 
                          Y (props.Rectangle.Pos.Y + 46.7)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "8px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "1"]
                    line [ X1 (props.Rectangle.Pos.X + 15.); Y1 (props.Rectangle.Pos.Y + 58.); X2 (props.Rectangle.Pos.X + 15.); Y2 (props.Rectangle.Pos.Y + 65.); Style[Stroke "Black"]] [] 

                    text
                        [ X (props.Rectangle.Pos.X + 20.) 
                          Y (props.Rectangle.Pos.Y - 20.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "20px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str props.Rectangle.Label]   

                else 
                    polygon
                        [ 
                            SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                        props.Rectangle.Pos.X props.Rectangle.Pos.Y
                                                        (props.Rectangle.Pos.X + 30.)(props.Rectangle.Pos.Y - 25.)
                                                        (props.Rectangle.Pos.X + 30.) (props.Rectangle.Pos.Y + 45.) 
                                                        (props.Rectangle.Pos.X) (props.Rectangle.Pos.Y + 21.7))
                            X props.Rectangle.Pos.X
                            Y props.Rectangle.Pos.Y
                            SVGAttr.Fill color
                            SVGAttr.FillOpacity 0.75
                            SVGAttr.Stroke "Black"
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]
                    
                    text
                        [ X (props.Rectangle.Pos.X + 25.) 
                          Y (props.Rectangle.Pos.Y )
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "8px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "0"]

                    text
                        [ X (props.Rectangle.Pos.X + 25.) 
                          Y (props.Rectangle.Pos.Y + 23.3)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "8px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "1"] 

                    line [ X1 (props.Rectangle.Pos.X + 15.); Y1 (props.Rectangle.Pos.Y + 33.); X2 (props.Rectangle.Pos.X + 15.); Y2 (props.Rectangle.Pos.Y + 40.); Style[Stroke "Black"]] []  
                    text
                        [ X (props.Rectangle.Pos.X + 10.) 
                          Y (props.Rectangle.Pos.Y - 40.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "20px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str props.Rectangle.Label]   
                                                                     
                ]
    )

let renderWire = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [                                                                                           
                if isMerge props.Rectangle.Type then
                    line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y); X2 (props.Rectangle.Pos.X + 20.); Y2 (props.Rectangle.Pos.Y); Style[Stroke "Black"]] []
                    line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y); X2 (props.Rectangle.Pos.X + 20.); Y2 (props.Rectangle.Pos.Y + 20.); Style[Stroke "Black"]] []
                    line [ X1 (props.Rectangle.Pos.X ); Y1 (props.Rectangle.Pos.Y + 20.); X2 (props.Rectangle.Pos.X + 20.); Y2 (props.Rectangle.Pos.Y + 20.); Style[Stroke "Black"]] []
                    line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y + 10.); X2 (props.Rectangle.Pos.X + 40.); Y2 (props.Rectangle.Pos.Y + 10.); Style[Stroke "indigo"; StrokeWidth "3"]] []
                else
                    line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y); X2 (props.Rectangle.Pos.X + 20.); Y2 (props.Rectangle.Pos.Y); Style[Stroke "indigo"; StrokeWidth "3"]] []
                    line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y ); X2 (props.Rectangle.Pos.X + 20.); Y2 (props.Rectangle.Pos.Y - 10.); Style[Stroke "indigo"; StrokeWidth "3"]] []
                    line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y - 10.); X2 (props.Rectangle.Pos.X + 40.); Y2 (props.Rectangle.Pos.Y - 10.); Style[Stroke "indigo"; StrokeWidth "3"]] []
                    line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y); X2 (props.Rectangle.Pos.X + 20.); Y2 (props.Rectangle.Pos.Y + 10.); Style[Stroke "Black"]] []
                    line [ X1 (props.Rectangle.Pos.X + 20.); Y1 (props.Rectangle.Pos.Y + 10.); X2 (props.Rectangle.Pos.X + 40.); Y2 (props.Rectangle.Pos.Y + 10.); Style[Stroke "Black"]] []

                text
                    [ X (props.Rectangle.Pos.X + 10.) 
                      Y (props.Rectangle.Pos.Y - 30.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   
            ]
    )

let renderRegister = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [
                rect
                    [ 
                        
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 125.
                        SVGAttr.Height 100.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
                text
                    [ X (props.Rectangle.Pos.X + 60.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 60.) 
                      Y (props.Rectangle.Pos.Y + 1.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "15px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "REG" + displaychar]
                
                //left symbols
                text
                    [ X (props.Rectangle.Pos.X + 20.) 
                      Y (props.Rectangle.Pos.Y + 50.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "data-in"]

                text
                    [ X (props.Rectangle.Pos.X + 18.) 
                      Y (props.Rectangle.Pos.Y + 80.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "12.5px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "clk"]
                //right symbol
                text
                    [ X (props.Rectangle.Pos.X + 100.) 
                      Y (props.Rectangle.Pos.Y + 50.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str "data-out"]
                //optional enable
                if IsEnable props.Rectangle.Type
                then
                    text
                        [ X (props.Rectangle.Pos.X + 60.) 
                          Y (props.Rectangle.Pos.Y + 85.)
                          Style
                                [
                                     TextAnchor "middle"
                                     DominantBaseline "hanging"
                                     FontSize "10px"
                                     FontWeight "Normal"
                                     Fill "Black"
                                 ]
                        ] 
                        [str "EN"]
                // clock indentation with two intersecting lines
                line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 80.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 85.); Style[Stroke "Black"]] []
                line [ X1 (props.Rectangle.Pos.X); Y1 (props.Rectangle.Pos.Y + 90.); X2 (props.Rectangle.Pos.X + 7.5); Y2 (props.Rectangle.Pos.Y + 85.); Style[Stroke "Black"]] []
            ]
    )

let renderBus = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        let bus = getBus props.Rectangle.Type
        let msb = fst(bus)
        let lsb = snd(bus)
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
            [   
                polygon
                    [ 
                        SVGAttr.Points (sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                                    props.Rectangle.Pos.X props.Rectangle.Pos.Y
                                                    (props.Rectangle.Pos.X + 30.) props.Rectangle.Pos.Y
                                                    (props.Rectangle.Pos.X + 35.) (props.Rectangle.Pos.Y + 7.) 
                                                    (props.Rectangle.Pos.X + 45.) (props.Rectangle.Pos.Y + 7.)
                                                    (props.Rectangle.Pos.X + 45.) (props.Rectangle.Pos.Y + 14.)
                                                    (props.Rectangle.Pos.X + 35.) (props.Rectangle.Pos.Y + 14.)
                                                    (props.Rectangle.Pos.X + 30.) (props.Rectangle.Pos.Y + 21.)
                                                    props.Rectangle.Pos.X (props.Rectangle.Pos.Y + 21.))
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
                text
                    [ X (props.Rectangle.Pos.X + 20.) 
                      Y (props.Rectangle.Pos.Y - 20.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str props.Rectangle.Label]   

                text
                    [ X (props.Rectangle.Pos.X + 15.) 
                      Y (props.Rectangle.Pos.Y + 5.)
                      Style
                            [
                                 TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "10px"
                                 FontWeight "Normal"
                                 Fill "Black"
                             ]
                    ] 
                    [str <| "[" + sprintf("%A") msb + ".." + sprintf("%A") lsb + "]"]
                ]
    )

let renderCustom = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
        let displaychar = getGateDisplayChar props.Rectangle.Type
        let inputList = getInputList props.Rectangle.Type
        let outputList = getOutputList props.Rectangle.Type
        let handleMouseMove =
            Hooks.useRef(fun (ev : Types.Event) ->
                let ev = ev :?> Types.MouseEvent
                // x,y coordinates here do not compensate for transform in Sheet
                // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                Dragging(props.Rectangle.Id, posOf ev.pageX ev.pageY)
                |> props.Dispatch
            )

        let color =
            if props.Rectangle.IsDragging then
                "green"
            else
                "grey"
        
        let generateCustom (inputList: string list) (outputList: string list) = 
            let printItxt index element = 
                text [ X (props.Rectangle.Pos.X + 5.)
                       Y ((props.Rectangle.Pos.Y) + 30. +  float (index * 20))
                       Style [ 
                                TextAnchor "middle"
                                DominantBaseline "hanging"
                                FontSize "15px"
                                FontWeight "Normal"
                                Fill "Black" ]
                             ]
                            [str <| sprintf "%A" element]
                            
            let printOtxt index element =     
                text [ X (props.Rectangle.Pos.X + 90.)
                       Y (props.Rectangle.Pos.Y + 30. +  float (index * 20))
                       Style [ 
                                TextAnchor "middle"
                                DominantBaseline "hanging"
                                FontSize "15px"
                                FontWeight "Normal"
                                Fill "Black" ] 
                              ] 
                            [str <| sprintf "%A" element]
                
            let block = 
                [
                  rect
                    [ 
                        X props.Rectangle.Pos.X
                        Y props.Rectangle.Pos.Y
                        SVGAttr.Width 100.
                        SVGAttr.Height 125.
                        SVGAttr.Fill color
                        SVGAttr.FillOpacity 0.75
                        SVGAttr.Stroke "Black"
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
                  text [ 
                         X (props.Rectangle.Pos.X + 50.)
                         Y (props.Rectangle.Pos.Y + 1.)
                         Style [ TextAnchor "middle"
                                 DominantBaseline "hanging"
                                 FontSize "20px"
                                 FontWeight "Normal"
                                 Fill "Black" ] ] 
                                 [str <| displaychar]                
                ] 
            List.mapi (printItxt) inputList |> List.append (List.mapi (printOtxt) outputList) |> List.append block
        g   [ 
                OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Rectangle.Id
                        |> props.Dispatch
                    )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Rectangle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ](Seq.ofList(generateCustom inputList outputList))
    )
/// View for one symbol with caching for efficient execution when input does not change
let private renderSymbol dispatch (sym: Symbol) =
    let symbolCategory = getSymbolCategory sym.Type
    match symbolCategory with 
    | Gates -> 
        renderGate({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Adder ->
        renderNbitsAdder({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | FlipFlop ->
        renderFlipFlop({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | IO ->
        renderIO({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Multiplexer ->
        renderMux({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Wire ->
        renderWire({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Registers ->
        renderRegister({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Buses ->
        renderBus({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Decoder ->
        renderDecoder({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | Constants ->
        renderConstant({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | ReadOnly ->
        renderROM({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | RandomAccess ->
        renderRAM({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    | CustomComponent ->
        renderCustom({
            Rectangle = sym
            Dispatch = dispatch
            key = (string) sym.Id
        })
    |_-> failwith "not implemented"


/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun sym -> renderSymbol dispatch sym)
    |> ofList

//---------------Other interface functions--------------------//

// Returns true if pos is within the bounds of the bounding box of the given symbol; else returns false -> Sheet
let isSymClicked (pos : XYPos) (sym : Symbol) : bool =
    match pos with
    | p when (p.X >= sym.BBox.TopLeft.X) && (p.X <= sym.BBox.BottomRight.X) &&
             (p.Y >= sym.BBox.TopLeft.Y) && (p.Y <= sym.BBox.BottomRight.Y)  
        -> true
    | _-> false

// Returns all PortInfo of all symbols in the model
let getAllPortInfo (symModel:Model) : PortInfo list = 
    symModel |> List.collect (fun sym -> sym.PortInfoList)

// Returns the bounding box of the symbol with the given Id
let getBoundingBoxOf (symModel: Model) (sId: CommonTypes.ComponentId) : BoundingBox option = 
    List.tryFind (fun sym -> sym.Id = sId) symModel
    |> Option.map (fun sym -> sym.BBox)

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

// Returns the portInfo of all ports of the symbol with the given Id
let getPortInfoOf (symModel: Model) (sId: CommonTypes.ComponentId) : PortInfo list =
    symModel
    |> List.filter (fun sym -> sym.Id = sId )
    |> List.collect (fun sym -> sym.PortInfoList)

let getBusWidth (symbol: Symbol) :  (int list) option =
    match symbol.Type with 
    | Input buswidth
    | NbitsAdder buswidth
    | SplitWire buswidth
    | Register buswidth
    | RegisterE buswidth
    | Output buswidth ->  Some [buswidth]
    | AsyncROM memory 
    | ROM memory
    | RAM memory -> Some [memory.AddressWidth; memory.WordWidth]
    |_-> None

// Returns the buswidth information of the symbol with the given id. 
// If the buswidth information not known at symbol creation, None is returned.
// For memory symbols, the first element is the address width, and the second element is the width of the data    
let getBusWidthOf (symModel: Model) (sId: CommonTypes.ComponentId) : (int list) option =
    symModel 
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> Option.bind getBusWidth

// Returns the Ids of the selected symbols
let getSelectedSymbolIds (symModel: Model) : ComponentId list = 
    symModel
    |> List.filter (fun sym -> isSymClicked sym.Pos sym)
    |> List.map (fun sym -> sym.Id)


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
