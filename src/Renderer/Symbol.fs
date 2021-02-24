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


/// Model to generate one symbol (skeleton). Id is a unique Id 
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should 
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to 
/// determine are they the same is fast.
type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType
    }

type Model = Symbol list

//----------------------------Message Type-----------------------------------//

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
    | AddSymbol of sTyp : CommonTypes.ComponentType * pagepos: XYPos // tells you the type of component it is and its starting position


//---------------------------------helper types and functions----------------//



let posDiff (a: XYPos) (b: XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a: XYPos) (b: XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
    /// 

let createNewSymbol (input: XYPos * CommonTypes.ComponentType) =
    let (pos, compTyp) = input
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this                          //comment this block later
        IsDragging = false // initial value can always be this
        Id = CommonTypes.ComponentId (Helpers.uuid()) 
        Type = compTyp
    }

/// Dummy function for test. The real init would probably have no symbols.
/// we just use the dummy function here as an intialiser like we did in tick 3
let init () = 
    [
        ({X = 100.; Y = 100.}, NbitsAdder(6));
        ({ X = 500.; Y = 100.}, NbitsAdder(4));
        ({X = 400.; Y = 500.}, NbitsAdder(5));
        ({X = 300.; Y = 600.}, And);
        ({X = 300.; Y = 700.}, Nand)
    ]
    |> List.map createNewSymbol
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (sTyp,pos) -> 
        (createNewSymbol (pos,sTyp)):: model, Cmd.none
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
    /// these are all the different types of symbols we can have here 
type RenderSymbolProps =                




///WHY WAS THIS MADE PRIVATE IN ORIGINAL CODE





    {
        Rectangle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
        Comp : CommonTypes.ComponentType
    }

let ANDfunc (props : RenderSymbolProps) (nand: bool)= 
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
                [str <| sprintf "&"]

            line [ X1 (props.Rectangle.Pos.X - 20.); Y1 (props.Rectangle.Pos.Y + 15.); X2 (props.Rectangle.Pos.X); Y2 (props.Rectangle.Pos.Y + 15.); Style[Stroke "Black"]] []
            line [ X1 (props.Rectangle.Pos.X - 20.); Y1 (props.Rectangle.Pos.Y + 60.); X2 (props.Rectangle.Pos.X); Y2 (props.Rectangle.Pos.Y + 60.); Style[Stroke "Black"]] []
            line [ X1 (props.Rectangle.Pos.X + 75.); Y1 (props.Rectangle.Pos.Y + 37.5); X2 (props.Rectangle.Pos.X + 95.); Y2 (props.Rectangle.Pos.Y + 37.5); Style[Stroke "Black"]] []
            if nand <> true
            then line [ X1 (props.Rectangle.Pos.X + 75.); Y1 (props.Rectangle.Pos.Y + 25.); X2 (props.Rectangle.Pos.X + 85.); Y2 (props.Rectangle.Pos.Y + 37.5); Style[Stroke "Black"]] []
        ]


/// View for one symbol with caching for efficient execution when input does not change
let private RenderSymbol (comp: CommonTypes.ComponentType)=
    match comp with
    | NbitsAdder x ->
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
                                    SVGAttr.Width 125.
                                    SVGAttr.Height 100.
                                    SVGAttr.Fill color
                                    SVGAttr.FillOpacity 0.4
                                    SVGAttr.Stroke "Black"
                                    SVGAttr.StrokeWidth 1
                                ]
                                [ ]

                            text
                                [ X (props.Rectangle.Pos.X + 60.) 
                                  Y props.Rectangle.Pos.Y
                                  Style
                                        [
                                             TextAnchor "middle"
                                             DominantBaseline "hanging"
                                             FontSize "15px"
                                             FontWeight "Normal"
                                             Fill "Black"
                                         ]
                                ] 
                                [str <| sprintf "adder(%d:0)" x]
                        ]
                            

                    
            )
    | And  ->
            FunctionComponent.Of(
                fun (props : RenderSymbolProps) -> (ANDfunc (props : RenderSymbolProps) true)
            )
    | Nand  ->
            FunctionComponent.Of(
                fun (props : RenderSymbolProps) -> (ANDfunc (props : RenderSymbolProps) false)
            )
    |_-> failwithf "not yet implemented"

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun rect ->
        RenderSymbol rect.Type
            {
            Rectangle = rect // name works for the demo!
            Dispatch = dispatch
            key= (string) rect.Id // special field used by react to detect whether lists have changed, set to symbol Id
            Comp = rect.Type
            }
    )
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)



/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: CommonTypes.ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
