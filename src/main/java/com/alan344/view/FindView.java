package com.alan344.view;

import com.alan344.bean.DataItem;
import com.alan344.bean.Table;
import com.alan344happyframework.util.StringUtils;
import javafx.scene.control.Label;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
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


    public void findTableByTableName(String tableNamePrefix, TreeItem<DataItem> DataRootTreeItem, TreeView<DataItem> treeViewDataSource) {
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
                        if (StringUtils.isNotEmpty(tableNamePrefix) && tableNameUpperCase.contains(tableNamePrefix)) {
                            int startIndex = tableNameUpperCase.indexOf(tableNamePrefix);
                            int endIndex = startIndex + tableNamePrefix.length();
                            if (startIndex != 0) {
                                Label label = new Label(tableName.substring(0, startIndex));
                                hBox.getChildren().add(label);
                            }

                            String prefix = tableName.substring(startIndex, endIndex);
                            Label label = new Label(prefix);
                            label.setStyle("-fx-background-color: orange");

                            String suffix = tableName.substring(endIndex);
                            Label label2 = new Label(suffix);
                            hBox.getChildren().addAll(label, label2);
                        } else {
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
