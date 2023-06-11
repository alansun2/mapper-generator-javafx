package com.alan344.component;

import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

/**
 * @author AlanSun
 * @date 2023/6/11 0:52
 */
public class SponsorStage extends BorderPane {

    private Stage stage;

    public SponsorStage(Stage ownerStage) {
        // if (stage == null) {
        Label label = new Label("请作者喝杯咖啡~");
        label.setStyle("-fx-text-fill: #81E069; -fx-font-weight: bold; -fx-font-size: 20");
        HBox hBox = new HBox(label);
        hBox.setStyle("-fx-padding: 10 0 10 0");
        hBox.setAlignment(Pos.CENTER);

        ImageView alipatImageView = new ImageView("image/sponsor/alipay.jpg");
        alipatImageView.setPreserveRatio(true);
        alipatImageView.setFitWidth(200);
        ImageView wechatImageView = new ImageView("image/sponsor/wechat.jpg");
        wechatImageView.setPreserveRatio(true);
        wechatImageView.setFitWidth(200);

        HBox hBox1 = new HBox(30, alipatImageView, wechatImageView);
        hBox1.setAlignment(Pos.CENTER);

        ImageView spin = new ImageView("image/sponsor/spin.gif");
        spin.setPreserveRatio(true);
        spin.setFitWidth(100);
        HBox hBox2 = new HBox(spin);
        hBox2.setAlignment(Pos.CENTER);
        hBox2.setStyle("-fx-padding: 10 0 10 0");

        this.getStylesheets().add("css/common.css");
        this.setPrefHeight(330);
        this.setPrefWidth(500);
        this.setTop(hBox);
        this.setCenter(hBox1);
        this.setBottom(hBox2);

        stage = new Stage();
        stage.setScene(new Scene(this));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/sponsor.png"));
        stage.setTitle("赞助");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initStyle(StageStyle.DECORATED);
        stage.initOwner(ownerStage);
        // }
        stage.show();
    }
}
