import * as Jquery from "jquery";
import * as React from 'react'
import StarRatings from 'react-star-ratings';

const PROFICIENCY_MAXIMUM_STARS = 5;

interface ICardProps {
    // Identifiers
    deckId: string;
    cardId: string;

    // Content
    title: string;
    shortDescription: string;
    score: number;
    enabled: boolean;
};

enum CardEnabledState {
    Enabled = "enabled",
    Disabled = "disabled",
    Updating = "updating",
};

interface ICardState {
    enabledState: CardEnabledState;
};

export class Card extends React.Component<ICardProps, ICardState> {
    constructor(props) {
        super(props);

        this.state = {
            enabledState: props.enabled ? CardEnabledState.Enabled : CardEnabledState.Disabled,
        };
    }

    computeProficiencyStars() {
        return this.props.score * PROFICIENCY_MAXIMUM_STARS;
    }

    setCardState(cardNewState: boolean) {
        if (cardNewState == true) {
            this.setState({
                enabledState: CardEnabledState.Enabled,
            });
        } else {
            this.setState({
                enabledState: CardEnabledState.Disabled,
            });
        }
    }

    persistCardState(cardNewState: boolean) {
        Jquery.ajax({
            url: "/api/v0/deck/" + this.props.deckId + "/setCardStatus/" + this.props.cardId + "/" + (cardNewState ? "enabled" : "disabled"),
            method: "POST",
            dataType: "text",
            statusCode: {
                401: () => {
                    alert("To enable cards, You must be signed in.");
                },
            }
        }).done(response => {
            this.setCardState(cardNewState);
        }).fail(response => {
            this.setCardState(!cardNewState);
        });
    }

    toggleEnabledState() {
        if (this.state.enabledState == CardEnabledState.Enabled) {
            this.persistCardState(false);
        } else if (this.state.enabledState == CardEnabledState.Disabled) {
            this.persistCardState(true);
        }

        this.setState({
            enabledState: CardEnabledState.Updating,
        });
    }

    render() {
        return (
            <div className={"card " + this.state.enabledState} onClick={this.toggleEnabledState.bind(this)}>
                <div className="card-header">
                    <div className="card-title">{ this.props.title }</div>
                    <div className="card-proficiency">
                        <StarRatings
                                  rating={this.computeProficiencyStars()}
                                  starRatedColor="rgb(230, 67, 47)"
                                  numberOfStars={PROFICIENCY_MAXIMUM_STARS}
                                  name='rating'
                                />
                    </div>
                </div>
                <div className="card-description">{ this.props.shortDescription }</div>
            </div>
        );
    }
};
