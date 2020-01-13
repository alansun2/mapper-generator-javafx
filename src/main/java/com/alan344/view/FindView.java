package com.alan344.view;

import com.alan344.bean.DataItem;
import com.alan344.bean.Table;
import com.alan344happyframework.util.StringUtils;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import org.springframework.stereotype.Service;

/**
 * @author AlanSun
 * @date 2020/1/11 9:39
 * 用于在 TreeView 中检索 table
 */
@Service
public class FindView {

    /**
     * table 列表搜索功能
     *
     * @param tableNamePrefix    键盘输入，用于匹配表
     * @param treeViewDataSource treeView
     */
    public void findTableByTableName(String tableNamePrefix, TreeView<DataItem> treeViewDataSource) {
        final MultipleSelectionModel<TreeItem<DataItem>> selectionModel = treeViewDataSource.getSelectionModel();
        final boolean[] isFirst = {true};
        treeViewDataSource.setCellFactory(param -> new TreeCell<DataItem>() {
            @Override
            protected void updateItem(DataItem item, boolean empty) {
                super.updateItem(item, empty);

                if (!empty) {
                    if (item instanceof Table) {
                        // table
                        HBox hBox = new HBox(new ImageView("/image/table.png"));
                        Table table = (Table) item;
                        String tableName = table.getTableName();
                        String tableNameUpperCase = tableName.toUpperCase();
                        // 判断是否存在搜索内容
                        if (StringUtils.isNotEmpty(tableNamePrefix) && tableNameUpperCase.contains(tableNamePrefix)) {

                            if (isFirst[0]) {
                                treeViewDataSource.scrollTo(this.getIndex());
                                selectionModel.clearAndSelect(this.getIndex());
                                isFirst[0] = false;
                            }
                            // 搜索内容在表名上的开始索引
                            int startIndex = tableNameUpperCase.indexOf(tableNamePrefix);
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
        });

    }
}
