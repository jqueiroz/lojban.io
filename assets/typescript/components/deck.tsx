import * as Jquery from "jquery";
import * as React from 'react'
import { Card } from './card'

interface IDeckProps {
    deckId: string;
};

interface IDeckState {
    deckExternalData: any;
};

export class Deck extends React.Component<IDeckProps, IDeckState> {
    componentDidMount() {
        Jquery.ajax({
            url: "/api/v0/deck/" + this.props.deckId,
            method: "GET",
            dataType: "json",
        }).done(deckExternalData => {
            console.log("Retrieved external data for deck:", deckExternalData);
            this.setState({deckExternalData});
        });
    }

    createCards() {
        let deckExternalData = this.state.deckExternalData;
        return deckExternalData.cards.map(cardExternalData => {
            let cardPreferences = deckExternalData.deckPreferences && deckExternalData.deckPreferences.cardPreferences;
            let cardProficiency = deckExternalData.deckProficiency && deckExternalData.deckProficiency.cardProficiency;
            let cardProps = {
                deckId: this.props.deckId,
                cardId: cardExternalData.title,
                title: cardExternalData.title,
                shortDescription: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor.",
                enabled: cardPreferences == null ? false : cardPreferences[cardExternalData.title].enabled,
                score: cardProficiency == null ? 0 : cardProficiency[cardExternalData.title].score,
            };
            return (
                <li key={cardProps.cardId}>
                    { React.createElement(Card, cardProps) }
                </li>
            );
        });
    }

    render() {
        if (this.state == null || this.state.deckExternalData == null) {
            return <div className="loading" />;
        }

        return (
            <ul>
                { this.createCards() }
            </ul>
        );
    }
};
