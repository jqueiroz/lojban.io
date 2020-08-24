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
    status: string;
};

enum CardStatus {
    AlreadyMastered = "AlreadyMastered",
    CurrentlyLearning = "CurrentlyLearning",
    NotStarted = "NotStarted",
    Updating = "Updating",
};

interface ICardState {
    status: CardStatus;
};

export class Card extends React.Component<ICardProps, ICardState> {
    constructor(props) {
        super(props);

        this.state = {
            status:
                props.status == CardStatus.AlreadyMastered.toString() ? CardStatus.AlreadyMastered : 
                props.status == CardStatus.CurrentlyLearning.toString() ? CardStatus.CurrentlyLearning :
                CardStatus.NotStarted,
        };
    }

    computeProficiencyStars() {
        return this.props.score * PROFICIENCY_MAXIMUM_STARS;
    }

    setCardStatus(cardNewStatus: CardStatus) {
        this.setState({
            status: cardNewStatus,
        });
    }

    persistNewCardStatus(cardPreviousStatus: CardStatus, cardNewStatus: CardStatus) {
        if (cardPreviousStatus == CardStatus.Updating || cardNewStatus === CardStatus.Updating) {
            throw new Error("Invalid card status for persisting.");
        }

        Jquery.ajax({
            url: "/api/v0/deck/" + this.props.deckId + "/setCardStatus/" + this.props.cardId + "/" + cardNewStatus.toString(),
            method: "POST",
            dataType: "text",
            statusCode: {
                401: () => {
                    alert("To manage cards, you need to sign in.");
                },
            }
        }).done(response => {
            this.setCardStatus(cardNewStatus);
        }).fail(response => {
            this.setCardStatus(cardPreviousStatus);
        });
    }

    getNextCardStatus(currentStatus) {
        if (this.state.status == CardStatus.AlreadyMastered) {
            return CardStatus.NotStarted;
        } else if (this.state.status == CardStatus.NotStarted) {
            return CardStatus.CurrentlyLearning;
        } else if (this.state.status == CardStatus.CurrentlyLearning) {
            return CardStatus.AlreadyMastered;
        } else {
            throw new Error("Invalid card status: " + currentStatus);
        }
    }

    toggleEnabledState() {
        this.persistNewCardStatus(this.state.status, this.getNextCardStatus(this.state.status));

        this.setState({
            status: CardStatus.Updating,
        });
    }

    render() {
        return (
            <div className={"card " + this.state.status} onClick={this.toggleEnabledState.bind(this)}>
                <div className="card-bg" />
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
                <div className="card-state" />
            </div>
        );
    }
};
