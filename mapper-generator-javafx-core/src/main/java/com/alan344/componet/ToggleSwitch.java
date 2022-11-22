package com.alan344.componet;

import javafx.animation.TranslateTransition;
import javafx.scene.control.Button;
import javafx.scene.layout.AnchorPane;
import javafx.util.Duration;

import java.util.function.Consumer;

/**
 * @author AlanSun
 * @date 2022/11/2 16:40
 */
public class ToggleSwitch extends AnchorPane {

    public ToggleSwitch(double width, boolean curToggle, Consumer<Boolean> consumer) {
        this.setPrefWidth(width);
        this.setMaxHeight(10);
        this.setPrefHeight(10);
        final double halfWidth = width / 2;
        this.setStyle((curToggle ? "-fx-background-color: #7070fd;" : "-fx-background-color: #d20a0a;") + "-fx-background-insets: 0; -fx-background-radius: 15");
        Button button = new Button(curToggle ? "开" : "关");
        button.setStyle("-fx-border-width: 0; -fx-background-radius: 15; -fx-background-insets: 0");
        button.setPrefWidth(halfWidth);
        button.setPrefHeight(10);
//        button.prefHeightProperty().bind(this.heightProperty());
        button.setLayoutX(curToggle ? 0 : halfWidth);
        button.setLayoutY(0);
        button.setOnAction(actionEvent -> {
            final Button source = (Button) actionEvent.getSource();
            TranslateTransition tt = new TranslateTransition();
            tt.setDuration(Duration.millis(100));
            tt.setNode(source);
            if (curToggle) {
                if (source.getTranslateX() == 0) {
                    this.close(source, tt, 0, halfWidth, consumer);
                } else {
                    this.open(source, tt, halfWidth, 0, consumer);
                }
            } else {
                if (source.getTranslateX() == 0) {
                    this.open(source, tt, 0, -halfWidth, consumer);
                } else {
                    this.close(source, tt, -halfWidth, 0, consumer);
                }
            }
            tt.play();
        });
        this.getChildren().add(button);
    }

    public void setPrefHeight(int prefHeight) {
        super.setPrefHeight(prefHeight);
    }

    private void open(Button source, TranslateTransition tt, double from, double to, Consumer<Boolean> consumer) {
        source.setText("开");
        tt.setFromX(from);
        tt.setFromY(0);
        tt.setToX(to);
        tt.setToY(0);
        this.setStyle("-fx-background-color: #7070fd; -fx-background-insets: 0; -fx-background-radius: 15");
        consumer.accept(true);
    }

    private void close(Button source, TranslateTransition tt, double from, double to, Consumer<Boolean> consumer) {
        source.setText("关");
        tt.setFromX(from);
        tt.setFromY(0);
        tt.setToX(to);
        tt.setToY(0);
        this.setStyle("-fx-background-color: #d20a0a; -fx-background-insets: 0; -fx-background-radius: 15");
        consumer.accept(false);
    }
}
