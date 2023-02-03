package com.alan344.view;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.componet.CustomTreeCell1;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;

/**
 * @author AlanSun
 * @date 2023/1/20 9:04
 */
public class MainView1 extends Application {

    @Override
    public void start(Stage primaryStage) {
        TreeView<DataItem> stringTreeView = new TreeView<>();
        // stringTreeView.setShowRoot(false);
        stringTreeView.setPrefWidth(100);
        stringTreeView.setCellFactory(CustomTreeCell1.forTreeView());
        final DataSource dataSourceRoot = new DataSource();
        dataSourceRoot.setConfigName("test");
        TreeItem<DataItem> root = new TreeItem<>(dataSourceRoot);

        stringTreeView.setRoot(root);

        final DataSource dataSource1 = new DataSource();
        dataSource1.setConfigName("1");
        final TreeItem<DataItem> item1 = new TreeItem<>(dataSource1);
        Table table11 = new Table();
        table11.setTableName("11");
        Table table12 = new Table();
        table12.setTableName("12");
        Table table13 = new Table();
        table13.setTableName("13");

        final TreeItem<DataItem> item11 = new TreeItem<>(table11);
        final TreeItem<DataItem> item12 = new TreeItem<>(table12);
        final TreeItem<DataItem> item13 = new TreeItem<>(table13);

        final DataSource dataSource2 = new DataSource();
        dataSource2.setConfigName("2");
        final TreeItem<DataItem> item2 = new TreeItem<>(dataSource2);
        Table table21 = new Table();
        table21.setTableName("21");
        Table table22 = new Table();
        table22.setTableName("22");
        Table table23 = new Table();
        table23.setTableName("23");
        final TreeItem<DataItem> item21 = new TreeItem<>(table21);
        final TreeItem<DataItem> item22 = new TreeItem<>(table22);
        final TreeItem<DataItem> item23 = new TreeItem<>(table23);

        root.getChildren().add(item1);
        root.getChildren().add(item2);

        item1.getChildren().add(item11);
        item1.getChildren().add(item12);
        item1.getChildren().add(item13);

        item2.getChildren().add(item21);
        item2.getChildren().add(item22);
        item2.getChildren().add(item23);

        // final DataSource dataSource3 = new DataSource();
        // dataSource3.setConfigName("3");
        // Table table31 = new Table();
        // table31.setTableName("31");
        // Table table32 = new Table();
        // table32.setTableName("32");
        // Table table33 = new Table();
        // table33.setTableName("33");
        // final TreeItem<DataItem> item3 = TreeUtils.add2Tree(dataSource3, root);
        // final TreeItem<DataItem> item31 = TreeUtils.add2Tree(table31, item3);
        // final TreeItem<DataItem> item32 = TreeUtils.add2Tree(table32, item3);
        // final TreeItem<DataItem> item33 = TreeUtils.add2Tree(table33, item3);

        AnchorPane borderPane = new AnchorPane(stringTreeView);
        stringTreeView.prefHeightProperty().bind(borderPane.heightProperty());
        primaryStage.setScene(new Scene(borderPane));
        primaryStage.setWidth(1200);
        primaryStage.setHeight(700);
        primaryStage.setTitle("Mybatis Friend");
        primaryStage.show();
    }
}
