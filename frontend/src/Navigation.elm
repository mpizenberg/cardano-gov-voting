module Navigation exposing (NavState, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias NavItem =
    { label : String
    , url : String
    , isActive : Bool
    }


type alias NavState =
    { isOpen : Bool
    }


type Msg
    = ToggleMobileMenu
    | CloseMenu


init : NavState
init =
    { isOpen = False
    }


update : Msg -> NavState -> (NavState, Cmd Msg)
update msg state =
    case msg of
        ToggleMobileMenu ->
            ({ state | isOpen = not state.isOpen }, Cmd.none)

        CloseMenu ->
            ({ state | isOpen = False }, Cmd.none)


navItems : List NavItem
navItems =
    [ { label = "Home", url = "/", isActive = False }
    , { label = "Proposals", url = "/proposals", isActive = False }
    , { label = "About", url = "/about", isActive = False }
    ]


view : { state : NavState, toMsg : Msg -> msg, brand : String, items : List { label : String, url : String, isActive : Bool } } -> Html msg
view config =
    nav [ class "relative z-10 w-full bg-white shadow-md" ]
        [ div [ class "container mx-auto px-4 py-6 xl:px-8" ]
            [ div [ class "flex items-center justify-between" ]
                -- Logo/Brand section
                [ div [ class "flex-shrink-0" ]
                    [ a [ href "/", class "flex items-center" ]
                        [ img [ src "/logo/foundation-logo.png", alt "Logo", class "h-4 w-auto mr-2" ] []
                        ]
                    ]
                
                -- Desktop menu
                , div [ class "hidden md:flex space-x-8" ] 
                    (List.map (viewDesktopMenuItem config.toMsg) config.items)
                
                -- Mobile menu button
                , div [ class "md:hidden" ]
                    [ button 
                        [ class "text-gray-600 hover:text-gray-900 focus:outline-none", 
                          onClick (config.toMsg ToggleMobileMenu) 
                        ]
                        [ -- Hamburger icon
                          div [ class "w-6 h-6 flex flex-col justify-around" ]
                            [ span [ class ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 " ++ 
                                if config.state.isOpen then "rotate-45 translate-y-1.5" else "") 
                              ] []
                            , span [ class ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 " ++ 
                                if config.state.isOpen then "opacity-0" else "") 
                              ] []
                            , span [ class ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 " ++ 
                                if config.state.isOpen then "-rotate-45 -translate-y-1.5" else "") 
                              ] []
                            ]
                        ]
                    ]
                ]
            
            -- Mobile menu (responsive)
            , div 
                [ class ("md:hidden transition-all duration-300 ease-in-out overflow-hidden " ++ 
                    if config.state.isOpen then "max-h-64" else "max-h-0")
                ]
                [ div [ class "py-2" ]
                    [ ul [ class "flex flex-col space-y-4 mt-4" ]
                        (List.map (viewMobileMenuItem config.toMsg) config.items)
                    ]
                ]
            ]
        ]

viewDesktopMenuItem : (Msg -> msg) -> { label : String, url : String, isActive : Bool } -> Html msg
viewDesktopMenuItem toMsg item =
    a [ href item.url, 
        class ("px-4 py-2 font-medium transition-all duration-200 " ++
            if item.isActive then 
                "text-gray-900 border-b-2 border-gray-800" 
            else 
                "text-gray-800 hover:text-gray-900 hover:border-b-2 hover:border-gray-800")
      ]
      [ text item.label ]


viewMobileMenuItem : (Msg -> msg) -> { label : String, url : String, isActive : Bool } -> Html msg
viewMobileMenuItem toMsg item =
    li []
        [ a [ href item.url, 
              class ("block px-4 py-2 rounded-md transition-colors duration-200 " ++
                if item.isActive then 
                    "text-gray-900 bg-gray-100" 
                else 
                    "text-gray-800 hover:bg-gray-100")
            ]
            [ text item.label ]
        ]
