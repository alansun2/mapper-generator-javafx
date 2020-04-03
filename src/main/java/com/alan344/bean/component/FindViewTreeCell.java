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
                final String tableName = item.toString();
                HBox hBox = new HBox(new ImageView("/image/table.png"), new Label(tableName));
                String tableNamePrefix = stringBuilder.toString();
                // 判断是否存在搜索内容
                if (StringUtils.isNotEmpty(tableNamePrefix)) {
                    if (tableName.contains(tableNamePrefix)) {
                        this.setGraphic(hBox);
                    } else {
                        this.setGraphic(null);
                    }
                } else {
                    this.setGraphic(hBox);
                }

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