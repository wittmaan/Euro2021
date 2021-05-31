package de.awi.tournamentsimulator.euro2021;

public class GroupStage {
    Integer points;
    Integer standing;

    public GroupStage(final Integer points) {
        this.points = points;
    }

    @Override
    public String toString() {
        return "GroupStage{" +
                "points=" + points +
                ", standing=" + standing +
                '}';
    }
}
