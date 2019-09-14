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
        return this.state.deckExternalData.cards.map(cardExternalData => {
            let cardProps = {
                title: cardExternalData.title,
                shortDescription: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor.",
                score: 0.5,
                enabled: true,
            };
            return (
                <li key={cardExternalData.title}>
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
