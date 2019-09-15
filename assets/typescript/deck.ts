import * as $ from "jquery";

import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { Deck } from './components/deck'

declare var deckId: string;

$(document).ready(function() {
    let root_element = $(".deck");
    let deckProps = {
        deckId: deckId,
    };
    let deckComponent = React.createElement(Deck, deckProps);
    ReactDOM.render(deckComponent, root_element[0]);
});
