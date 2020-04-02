package com.alan344.bean.component;

import com.alan344.bean.DataItem;
import com.alan344.bean.Table;
import com.alan344happyframework.util.StringUtils;
import javafx.scene.control.Label;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;

/**
 * @author AlanSun
 * @date 2020/4/2 16:47
 * 用于在 TreeView 中检索 table
 */
public class FindViewTreeCell extends TreeCell<DataItem> {
    private StringBuilder stringBuilder;
    private TreeView<DataItem> treeViewDataSource;

    public FindViewTreeCell(StringBuilder stringBuilder, TreeView<DataItem> treeViewDataSource) {
        this.stringBuilder = stringBuilder;
        this.treeViewDataSource = treeViewDataSource;
    }

    @Override
    protected void updateItem(DataItem item, boolean empty) {
        super.updateItem(item, empty);

        if (!empty) {
            if (item instanceof Table) {
                // table
                HBox hBox = new HBox(new ImageView("/image/table.png"));
                Table table = (Table) item;
                String tableName = table.getTableName();
                String tableNamePrefix = stringBuilder.toString();
                // 判断是否存在搜索内容
                if (StringUtils.isNotEmpty(tableNamePrefix) && tableName.contains(tableNamePrefix)) {

                    treeViewDataSource.scrollTo(this.getIndex());
                    treeViewDataSource.getSelectionModel().clearAndSelect(this.getIndex());

                    // 搜索内容在表名上的开始索引
                    int startIndex = tableName.indexOf(tableNamePrefix);
                    // 搜索内容在表名上的结束索引
                    int endIndex = startIndex + tableNamePrefix.length();
                    // 如果开始索引不是从 0 开始，则截取索引前的内容
                    if (startIndex != 0) {
                        Label prefix = new Label(tableName.substring(0, startIndex));
                        hBox.getChildren().add(prefix);
                    }
                    // 截取匹配内容，并修改
                    String match = tableName.substring(startIndex, endIndex);
                    Label label = new Label(match);
                    label.setStyle("-fx-background-color: orange");

                    // 匹配内容之后的字符
                    String suffix = tableName.substring(endIndex);
                    Label label2 = new Label(suffix);

                    hBox.getChildren().addAll(label, label2);
                } else {
                    // 未匹配
                    hBox.getChildren().add(new Label(tableName));
                }
                this.setGraphic(hBox);
            } else {
                // 数据库
                HBox hBox = new HBox(new ImageView("/image/database.png"), new Label(item.toString()));
                this.setGraphic(hBox);
            }
        } else {
            this.setGraphic(null);
        }
    }
}