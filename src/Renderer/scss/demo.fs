module Symbol
open CommonTypes
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

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

// Define bounding boxes to make symbols clickable
type BoundingBox = {
    P1: XYPos
    P2: XYPos
}

type PortId = | PortId of string
type PortType = Input | Output
type Orientation = Zero | Ninety | OneEighty | TwoSeventy

type Port = {
    Id: PortId
    PortType: PortType
    Pos: XYPos
    BoundingBox: BoundingBox
    Width: int
    IsHighlighted: bool
}

type Symbol =
    {
        Pos: XYPos
        LastDragPos: XYPos
        IsDragging: bool
        Id: CommonTypes.ComponentId
        IsShowingPorts: bool
        IsHighlighted: bool
        DistanceFromPointer: float
        IsTransparent: bool
        BoundingBox: BoundingBox
        Ports: Port list
    }

// A model is simply a collection of symbols.
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
    | AddCircle of XYPos // used by demo code to add a circle
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface


//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (pos:XYPos) (tp: ComponentType) =
    {
        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        Id = CommonTypes.ComponentId (Helpers.uuid()) // create a unique id for this symbol
        IsShowingPorts = false
        IsHighlighted = false
        DistanceFromPointer = infinity
        IsTransparent = false
        BoundingBox = {
            P1 = pos
            P2 = pos
        }
        Ports = [
            {
                Id = PortId (uuid())
                PortType = PortType.Input
                Pos = {pos with X = pos.X - 10.}
                BoundingBox = {
                    P1 = {pos with X = pos.X - 10.}
                    P2 = {X = pos.X - 10.; Y = pos.Y + 10.}
                }
                Width = 5
                IsHighlighted = false
            }
            {
                Id = PortId (uuid())
                PortType = PortType.Output
                Pos = {pos with X = pos.X + 10.}
                BoundingBox = {
                    P1 = {pos with X = pos.X + 10.}
                    P2 = {X = pos.X + 10.; Y = pos.Y + 10.}
                }
                Width = 5
                IsHighlighted = false
            }
        ]
    }


/// Dummy function for test. The real init would probably have no symbols.
let init () =
    // List.allPairs [1..14] [1..14]
    // |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    [({X = 100.0; Y = 100.0}, And(3))
     {(X = 500.0; Y = 100.0}, Xor)
     {X = 400.0; Y = 500.0}]
    ||> List.map createNewSymbol
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddCircle pos -> 
        createNewSymbol pos :: model, Cmd.none
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
                    Ports = List.map (fun port ->
                            {port with
                                Pos = posAdd port.Pos diff
                                BoundingBox = {
                                    P1 = posAdd port.BoundingBox.P1 diff
                                    P2 = posAdd port.BoundingBox.P2 diff
                                }
                            }
                        ) sym.Ports
                    BoundingBox = {
                        P1 = posAdd sym.BoundingBox.P1 diff
                        P2 = posAdd sym.BoundingBox.P2 diff
                    }
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
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messages
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderComponentProps =
    {
        Component : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderComponent =
    FunctionComponent.Of(
        fun (props : RenderComponentProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Component.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Component.IsDragging then
                    "lightblue"
                else
                    "grey"
            
            circle [
                        OnMouseUp (fun ev -> 
                                    document.removeEventListener("mousemove", handleMouseMove.current)
                                    EndDragging props.Component.Id
                                    |> props.Dispatch
                                )
                        OnMouseDown (fun ev -> 
                            // See note above re coords wrong if zoom <> 1.0
                            StartDragging (props.Component.Id, posOf ev.pageX ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        Cx (props.Component.Pos.X - 20.)
                        Cy props.Component.Pos.Y
                        R 20.
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]
    , "Component"
    , equalsButFunctions
    )
    
let private renderPorts =
    FunctionComponent.Of(
        fun (props : RenderComponentProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Component.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Component.IsDragging then
                    "lightblue"
                else
                    "grey"
            
            g [
                OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Component.Id
                            |> props.Dis[13:00, 23/02/2021] Arman:   )
                OnMouseDown (fun ev -> 
                    // See note above re coords wrong if zoom <> 1.0
                    StartDragging (props.Component.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                    document.addEventListener("mousemove", handleMouseMove.current)
                )
            ]
                [
                    // rect
                    //     [
                    //         
                    //     ]
                    //     [ ]
                    
                    circle
                        [ 
                            Cx (props.Component.Pos.X - 20.)
                            Cy props.Component.Pos.Y
                            R 20.
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]
                    
                    circle
                        [ 
                            Cx (props.Component.Pos.X + 20.)
                            Cy props.Component.Pos.Y
                            R 20.
                            SVGAttr.Fill color
                            SVGAttr.Stroke color
                            SVGAttr.StrokeWidth 1
                        ]
                        [ ]
                ]
    , "Ports"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as comp) ->
        renderComponent 
            {
                Component = comp
                Dispatch = dispatch
                key = id
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