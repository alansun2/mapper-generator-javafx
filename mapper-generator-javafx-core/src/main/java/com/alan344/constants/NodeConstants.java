package com.alan344.constants;

import javafx.application.HostServices;
import javafx.scene.control.ListView;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;


/**
 * @author AlanSun
 * @date 2020/4/7 11:31
 */
public class NodeConstants {
    public static Stage primaryStage;

    public static HostServices hostServices;

    public static BorderPane borderPaneWrap;

    /**
     * 右边展示区域第一个 borderPane
     */
    public static BorderPane borderPane1;

    /**
     * mybatis 配置时的 listView
     */
    public static ListView<VBox> mybatisListView;
}
