package com.alan344.service;

import com.alan344.bean.DataItem;
import com.alan344.utils.TreeUtils;
import javafx.scene.control.TreeItem;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author AlanSun
 * @date 2019/8/9 9:51
 */
@Slf4j
@Service
public class MainService {

    @Autowired
    private TableService tableService;

    public TreeItem<DataItem> add2Tree(DataItem dataItem, TreeItem<DataItem> dataSourceTreeItemRoot) {
        TreeItem<DataItem> dataSourceTreeItem = TreeUtils.add2Tree(dataItem, dataSourceTreeItemRoot);
        //添加展开监听
        dataSourceTreeItem.addEventHandler(TreeItem.<DataItem>branchExpandedEvent(), event -> {
            //没有则区远程拉去数据库表列表
            tableService.loadTables(event.getTreeItem());
        });

        return dataSourceTreeItem;
    }
}
