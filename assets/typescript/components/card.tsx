import * as React from 'react'
import StarRatings from 'react-star-ratings';

interface ICardProps {
    title: string;
    shortDescription: string;
    score: number;
    enabled: boolean;
};

enum CardEnabledState {
    Enabled = "enabled",
    Disabled = "disabled",
    Updating = "updating",
}

interface ICardState {
    enabledState: CardEnabledState;
}

const PROFICIENCY_MAXIMUM_STARS = 5;

export class Card extends React.Component<ICardProps, ICardState> {
    constructor(props) {
        super(props);

        this.state = {
            enabledState: props.enabled ? CardEnabledState.Enabled : CardEnabledState.Disabled,
        };

        this.toggleEnabledState = this.toggleEnabledState.bind(this);
    }

    computeProficiencyStars() {
        return this.props.score * PROFICIENCY_MAXIMUM_STARS;
    }

    toggleEnabledState() {
        if (this.state.enabledState == CardEnabledState.Enabled) {
            this.setState({
                enabledState: CardEnabledState.Disabled,
            });
        }
        else if (this.state.enabledState == CardEnabledState.Disabled) {
            this.setState({
                enabledState: CardEnabledState.Enabled,
            });
        }
    }

    render() {
        return (
            <div className={"card " + this.state.enabledState} onClick={this.toggleEnabledState}>
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
