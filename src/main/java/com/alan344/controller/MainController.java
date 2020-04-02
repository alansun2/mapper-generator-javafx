package com.alan344.controller;

import com.alan344.bean.DataItem;
import com.alan344.init.DataSourceTreeItemInit;
import com.alan344.init.DataSourceTreeViewInit;
import com.alan344.init.MapperCheckBoxInit;
import com.alan344.init.RightListViewInit;
import com.alan344.service.ColumnService;
import com.alan344.utils.DialogUtils;
import javafx.application.HostServices;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/7 17:04
 */
@Controller
public class MainController implements Initializable {
    /**
     * 主布局控件
     */
    @FXML
    private BorderPane borderPane;

    /**
     * 右边的 border
     */
    @FXML
    private BorderPane borderPane1;

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

    @Getter
    @FXML
    private ListView<VBox> vBoxListView;

    /**
     * 搜索小时用的 label
     */
    @FXML
    private Label tableFindLabel;

    /**
     * 右边 border 固定再上面的 两个 HBox。存放 checkBox
     */
    @FXML
    private HBox mapperCheckBoxHBox1;

    @FXML
    private HBox mapperCheckBoxHBox2;

    //-------------------------------service----------------------------------------------------------------------------

    @Resource
    private DataSourceController dataSourceController;

    @Resource
    private ConfigController configController;

    @Resource
    private AboutController aboutController;

    @Resource
    private ColumnService columnService;

    @Resource
    private BeanFactory beanFactory;

    // -------------------------init----------------------------------------------------------------------------------//
    @Resource
    private DataSourceTreeItemInit dataSourceTreeItemInit;

    @Resource
    private DataSourceTreeViewInit dataSourceTreeViewInit;

    @Resource
    private RightListViewInit rightListViewInit;

    @Resource
    private MapperCheckBoxInit mapperCheckBoxInit;

    //--------------------------------init start----------------------------------------------------------------------//

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        // 把菜单的长度和主布局控件绑定
        menuBar.prefWidthProperty().bind(borderPane.widthProperty());

        dataSourceTreeViewInit.treeViewInit(treeViewDataSource, borderPane1, borderPane, treeItemDataSourceRoot, vBoxListView);

        // init mapperCheckBox
        mapperCheckBoxInit.checkBoxInit(mapperCheckBoxHBox1, mapperCheckBoxHBox2);

        // 从文件加载数据源至pane
        dataSourceTreeItemInit.initLoadData(treeItemDataSourceRoot);
        // 添加表搜索监听
        dataSourceTreeItemInit.addListenOnDataSourceBorderPane(treeViewDataSource, tableFindLabel);
    }

    //--------------------------------init end------------------------------------------------------------------------//

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() throws IOException {
        dataSourceController.addDataSource((Stage) borderPane.getScene().getWindow());
    }

    /**
     * 退出时弹出询问
     */
    @FXML
    public void exit() {
//        Platform.exit(); //直接退出
        DialogUtils.closeDialog((Stage) borderPane.getScene().getWindow());
    }

    /**
     * 点开项目地址
     */
    @FXML
    public void openGithub() {
        HostServices hostServices = beanFactory.getBean(HostServices.class);
        hostServices.showDocument("https://github.com/alansun2/mapper-generator-javafx");
    }

    /**
     * 刷新 table 的字段信息
     */
    @FXML
    private void refreshTableColumn() {
        VBox selectedItemVBox = vBoxListView.getSelectionModel().getSelectedItem();
        String tableName = ((Label) ((HBox) selectedItemVBox.getChildren().get(0)).getChildren().get(0)).getText();
        columnService.reloadColumns(tableName);
        selectedItemVBox.getChildren().remove(2);
        rightListViewInit.expandTableViewColumns(selectedItemVBox);
    }

    //-------------

    /**
     * 导出窗口
     *
     * @throws IOException e
     */
    @FXML
    public void openConfigWindow() throws IOException {
        configController.openConfigPane((Stage) borderPane.getScene().getWindow());
    }

    /**
     * 关于窗口
     *
     * @throws IOException e
     */
    @FXML
    public void openAboutWindow() throws IOException {
        aboutController.openWindow((Stage) borderPane.getScene().getWindow());
    }
}
