package com.alan344.utils;

import com.alan344.exception.BizException;
import javafx.animation.KeyFrame;
import javafx.animation.KeyValue;
import javafx.animation.Timeline;
import javafx.scene.Scene;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.util.Duration;

/**
 * @author AlanSun
 * @date 2019/8/16 18:03
 */
public class Toast {
    public static void makeText(Stage stage, String message, final int displayTime, int fadeInDelay, final int fadeOutDelay, double size, double opacity) {
        final Stage toastStage = new Stage();
        toastStage.initOwner(stage);
        toastStage.setResizable(false);
        toastStage.initStyle(StageStyle.TRANSPARENT);
        Text text = new Text(message);
        text.setFont(Font.font(size));
        text.setFill(Color.RED);
        StackPane root = new StackPane(text);
        root.setStyle("-fx-background-radius: 5; -fx-background-color: rgb(232,232,232, 0.5); -fx-padding: 10px;");
        root.setOpacity(opacity);
        Scene scene = new Scene(root);
        scene.setFill(Color.TRANSPARENT);
        toastStage.setScene(scene);
        toastStage.show();
        Timeline fadeInTimeline = new Timeline();
        Duration duration1 = Duration.millis(fadeInDelay);
        KeyValue[] var10003 = new KeyValue[1];
        Scene var10008 = toastStage.getScene();
        var10003[0] = new KeyValue(var10008.getRoot().opacityProperty(), 1);
        KeyFrame fadeInKey1 = new KeyFrame(duration1, var10003);
        fadeInTimeline.getKeyFrames().add(fadeInKey1);
        fadeInTimeline.setOnFinished(event -> (new Thread((() -> {
            try {
                Thread.sleep(displayTime);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            Timeline fadeOutTimeline = new Timeline();
            Duration duration = Duration.millis(fadeOutDelay);
            KeyValue[] var100031 = new KeyValue[1];
            Scene scene1 = toastStage.getScene();
            var100031[0] = new KeyValue(scene1.getRoot().opacityProperty(), 0);
            KeyFrame fadeOutKey1 = new KeyFrame(duration, var100031);
            fadeOutTimeline.getKeyFrames().add(fadeOutKey1);
            fadeOutTimeline.setOnFinished(event1 -> {
                toastStage.close();
            });
            fadeOutTimeline.play();
        }))).start());
        fadeInTimeline.play();
        throw new BizException(message);
    }

    public static void makeTextDefault(Stage stage, String message) {
        Toast.makeText(stage, message, 3000, 500, 500, 12, 5);
        throw new BizException(message);
    }
}
