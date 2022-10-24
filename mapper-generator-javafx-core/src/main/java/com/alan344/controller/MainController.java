package com.alan344.controller;

import com.alan344.bean.DataItem;
import com.alan344.constants.NodeConstants;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.init.DataSourceTreeItemInit;
import com.alan344.init.DataSourceTreeViewInit;
import com.alan344.init.FindTableInit;
import com.alan344.utils.DialogUtils;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.MenuBar;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.BorderPane;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/7 17:04
 */
@Slf4j
@Controller
public class MainController implements Initializable {
    /**
     * 主布局控件
     */
    @Getter
    @FXML
    private BorderPane borderPaneMain;

    /**
     * 右边的父 border
     */
    @FXML
    private BorderPane borderPaneWrap;

    @FXML
    private MenuBar menuBar;

    /**
     * 左边的 treeView
     */
    @Getter
    @FXML
    private TreeView<DataItem> treeViewDataSource;

    /**
     * 根数据源
     */
    @Getter
    @FXML
    private TreeItem<DataItem> treeItemDataSourceRoot;

    /**
     * 搜索表时用的 TextField
     */
    @Getter
    @FXML
    private TextField tableFindTextField;

    //-------------------------------service----------------------------------------------------------------------------

    @Resource
    private DataSourceController dataSourceController;

    @Resource
    private AboutController aboutController;

    @Resource
    private BeanFactory beanFactory;

    // -------------------------init----------------------------------------------------------------------------------//

    @Resource
    private DataSourceTreeItemInit dataSourceTreeItemInit;

    @Resource
    private DataSourceTreeViewInit dataSourceTreeViewInit;

    @Resource
    private FindTableInit findTableInit;

    //--------------------------------init----------------------------------------------------------------------//

    @SneakyThrows
    @Override
    public void initialize(URL location, ResourceBundle resources) {
        NodeConstants.borderPaneWrap = borderPaneWrap;
        // 把菜单的长度和主布局控件绑定
        menuBar.prefWidthProperty().bind(borderPaneMain.widthProperty());

        dataSourceTreeViewInit.treeViewInit(treeViewDataSource);

        // 从文件加载数据源至pane
        dataSourceTreeItemInit.initLoadData(treeItemDataSourceRoot);

        // 添加表搜索监听
        findTableInit.addListener(treeViewDataSource, tableFindTextField, borderPaneWrap);

        // 加载右边中间的 borderPane
        FxmlLoadFactory.create("/fxml/mybatis-list-view.fxml", beanFactory);
    }

    //--------------------------------init end------------------------------------------------------------------------//

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() {
        dataSourceController.openDataSourceSetUp(NodeConstants.primaryStage, null);
    }

    /**
     * 退出时弹出询问
     */
    @FXML
    public void exit() {
        DialogUtils.closeDialog(NodeConstants.primaryStage);
    }

    /**
     * 点开项目地址
     */
    @FXML
    public void openGithub() {
        NodeConstants.hostServices.showDocument("https://github.com/alansun2/mapper-generator-javafx");
    }

    /**
     * 关于窗口
     */
    @FXML
    public void openAboutWindow() {
        aboutController.openWindow(NodeConstants.primaryStage);
    }
}
