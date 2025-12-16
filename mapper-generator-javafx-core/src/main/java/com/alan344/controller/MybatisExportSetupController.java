package com.alan344.controller;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alan344.bean.config.MybatisExportConfig;
import com.alan344.component.FileSelectTextHBox;
import com.alan344.component.FileSelectTextToggleHBox;
import com.alan344.component.LeftRightLinkageBorderPane;
import com.alan344.component.MybatisExportGroupItemHBox;
import com.alan344.component.MybatisExportItemHBox;
import com.alan344.component.PropertyPane;
import com.alan344.constants.BaseConstants;
import com.alan344.constants.NodeConstants;
import com.alan344.constants.enums.FileWriteModeEnum;
import com.alan344.constants.enums.FrameworkTypeEnum;
import com.alan344.constants.enums.JavaClientTypeEnum;
import com.alan344.constants.enums.LanguageEnum;
import com.alan344.constants.enums.TargetNameEnum;
import com.alan344.factory.DialogFactory;
import com.alan344.factory.FileDirChooserFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import com.alan344.utils.Assert;
import com.alan344.utils.StringUtils;
import com.alan344.utils.Toast;
import com.jfoenix.controls.JFXCheckBox;
import com.jfoenix.controls.JFXComboBox;
import javafx.collections.FXCollections;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import lombok.extern.slf4j.Slf4j;
import org.controlsfx.validation.ValidationResult;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.kordamp.ikonli.javafx.FontIcon;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.File;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * @author AlanSun
 * @since 2019/8/15 17:55
 */
@Slf4j
@Controller
public class MybatisExportSetupController {
    @Autowired
    private ConfigService configService;
    @Autowired
    private ExportService exportService;
    @Autowired
    private ExtraFileController extraFileController;
    @Autowired
    private MybatisAdvanceSetController mybatisAdvanceSetController;

    private String currentMapperRootInterface;

    private final Map<String, Region> configNameBorderPaneMap = new HashMap<>();
    private final Map<String, ValidationSupport> configNameValidationMap = new HashMap<>() {
        @Override
        public ValidationSupport get(Object key) {
            ValidationSupport validationSupport = super.get(key);
            if (validationSupport == null) {
                validationSupport = new ValidationSupport();
                validationSupport.validationDecoratorProperty().set(styleClassValidationDecoration);
                this.put((String) key, validationSupport);
            }
            return validationSupport;
        }
    };

    final StyleClassValidationDecoration styleClassValidationDecoration = new StyleClassValidationDecoration();
    private final NodeHandler nodeHandler = NodeHandler.getSingleTon(true);
    private final Map<String, LeftRightLinkageBorderPane<MybatisExportConfig, MybatisExportGroupItemHBox>> CACHE =
            new HashMap<>();

    private LeftRightLinkageBorderPane<MybatisExportConfig, MybatisExportGroupItemHBox> linkageBorderPane;

    public BorderPane getBorderPane(String configName) {
        linkageBorderPane = CACHE.computeIfAbsent(configName, s -> {
            final LeftRightLinkageBorderPane<MybatisExportConfig, MybatisExportGroupItemHBox> linkageBorderPane1 =
                    new LeftRightLinkageBorderPane<>(
                            MybatisExportConfig::new,
                            MybatisExportGroupItemHBox::new,
                            this::getExportSetupRegion,
                            NodeConstants.primaryStage,
                            this.getBottomBtns(),
                            0.25
                    );

            linkageBorderPane1.addLeftItems(configService.loadConfigFromFile());
            return linkageBorderPane1;
        });
        return linkageBorderPane;
    }

    private List<Button> getBottomBtns() {
        Button openExtraPropertyStageBtn = new Button("添加额外属性");
        openExtraPropertyStageBtn.setOnAction(event -> this.openExtraFileCustomProperties());
        openExtraPropertyStageBtn.setPrefWidth(100);
        Button saveBtn = new Button("保存");
        saveBtn.setOnAction(event -> {
            this.valid(this.getCurrentConfig().getConfigName());
            exportService.saveSetup();
            DialogFactory.successDialog(NodeConstants.primaryStage, "保存", "成功");
        });
        saveBtn.setPrefWidth(70);
        Button exportBtn = new Button("导出");
        exportBtn.getStyleClass().add("export");
        exportBtn.setOnAction(event -> this.export());
        exportBtn.setPrefWidth(70);
        Button nextBtn = new Button("下一步");
        nextBtn.setOnAction(event -> this.next());
        nextBtn.setPrefWidth(70);
        Button preBtn = new Button("返回");
        preBtn.setOnAction(event -> this.pre());
        preBtn.setPrefWidth(70);
        return List.of(openExtraPropertyStageBtn, saveBtn, exportBtn, nextBtn, preBtn);
    }

    /**
     * 打开添加自定义属性页面
     */
    public void openExtraFileCustomProperties() {
        final MybatisExportGroupItemHBox selectedItem =
                linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
        if (null == selectedItem) {
            Toast.makeTextDefault(NodeConstants.primaryStage, "请先新增配置");
        }

        final MybatisExportConfig config = Objects.requireNonNull(selectedItem).getConfig();
        LinkedHashMap<String, String> customProperties = config.getCustomProperties();
        if (null == customProperties) {
            customProperties = new LinkedHashMap<>();
            config.setCustomProperties(customProperties);
        }
        PropertyPane.open(customProperties);
    }

    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    public void next() {
        final MybatisExportConfig config = this.getCurrentConfig();
        this.valid(config.getConfigName());

        config.setExportExtraFile(true);

        BaseConstants.currentConfig = config;

        Node next = extraFileController.getBorderPane(config);
        // 入栈
        nodeHandler.addNode(next);

        NodeConstants.borderPaneWrap.setCenter(next);
    }

    public void export() {
        final MybatisExportConfig config = this.getCurrentConfig();
        this.valid(config.getConfigName());

        config.setExportExtraFile(false);

        BaseConstants.currentConfig = config;
        exportService.export(config);
    }

    public Region getExportSetupRegion(MybatisExportConfig mybatisExportConfig) {
        return configNameBorderPaneMap.computeIfAbsent(mybatisExportConfig.getConfigName(), s -> {
            final ValidationSupport validationSupport = configNameValidationMap.get(s);
            SplitPane splitPane = new SplitPane();
            splitPane.getStylesheets().add("css/common.css");
            splitPane.setDividerPositions(0.78);
            splitPane.setOrientation(Orientation.VERTICAL);
            ListView<HBox> hBoxListView = new ListView<>();
            hBoxListView.prefWidthProperty().bind(splitPane.widthProperty());

            BorderPane borderPane = new BorderPane();
            splitPane.getItems().addAll(hBoxListView, borderPane);

            TextField configNameText = new TextField(mybatisExportConfig.getConfigName());
            mybatisExportConfig.configNameProperty().bindBidirectional(configNameText.textProperty());
            MybatisExportItemHBox configNameHbox = new MybatisExportItemHBox("配置名称:", configNameText);
            validationSupport.registerValidator(configNameText, Validator.createEmptyValidator("配置名称必填"));
            configNameText.textProperty().addListener((observableValue, s1, t1) -> {
                // 更新验证, 否则点击下一步时无法获取验证器
                final ValidationSupport validationSupport1 = configNameValidationMap.get(s1);
                configNameValidationMap.remove(s1);
                configNameValidationMap.put(t1, validationSupport1);
            });

            TextField authorText = new TextField(mybatisExportConfig.getAuthor());
            mybatisExportConfig.authorProperty().bindBidirectional(authorText.textProperty());
            MybatisExportItemHBox authorHbox = new MybatisExportItemHBox("作者名称:", authorText);
            validationSupport.registerValidator(authorText, Validator.createEmptyValidator("作者名称必填"));

            JFXComboBox<String> writeFileComBox =
                    new JFXComboBox<>(FXCollections.observableArrayList(FileWriteModeEnum.valuesToString()));
            writeFileComBox.valueProperty().addListener((observable, oldValue, newValue) -> {
                if (null != newValue) {
                    mybatisExportConfig.setWriteMode(FileWriteModeEnum.getEnum(newValue));
                }
            });
            writeFileComBox.setValue(mybatisExportConfig.getWriteMode().getValue());
            MybatisExportItemHBox writeFileHbox = new MybatisExportItemHBox("文件写入模式:", writeFileComBox);

            FileSelectTextHBox projectDirText = new FileSelectTextHBox("浏览", mybatisExportConfig.getProjectDir());
            mybatisExportConfig.projectDirProperty().bindBidirectional(projectDirText.getTextField().textProperty());
            MybatisExportItemHBox projectDirHbox = new MybatisExportItemHBox("项目地址:", projectDirText);
            projectDirText.onAction(event -> this.beanDirectoryScan(projectDirText));
            validationSupport.registerValidator(projectDirText.getTextField(), Validator.createEmptyValidator(
                    "项目地址必填"));

            TextField projectNameText = new TextField(mybatisExportConfig.getProjectName());
            projectNameText.setEditable(false);
            projectNameText.setStyle("-fx-background-color: #f4f4f4;");
            mybatisExportConfig.projectNameProperty().bindBidirectional(projectNameText.textProperty());
            MybatisExportItemHBox projectNameHbox = new MybatisExportItemHBox("项目名称:", projectNameText);
            validationSupport.registerValidator(projectNameText, Validator.createEmptyValidator("项目名称必填"));

            projectDirText.getTextField().textProperty().addListener((observable, oldValue, newValue) -> projectNameText.setText(FileUtil.getName(newValue)));

            JFXComboBox<LanguageEnum> languageComboBox =
                    new JFXComboBox<>(FXCollections.observableArrayList(LanguageEnum.values()));
            MybatisExportItemHBox languageHbox = new MybatisExportItemHBox("语言:", languageComboBox);

            JFXComboBox<FrameworkTypeEnum> frameworkComboBox =
                    new JFXComboBox<>(FXCollections.observableArrayList(FrameworkTypeEnum.values()));
            MybatisExportItemHBox frameworkHbox = new MybatisExportItemHBox("框架:", frameworkComboBox);

            FileSelectTextToggleHBox beanLocationText = new FileSelectTextToggleHBox("浏览",
                    mybatisExportConfig.modelEnableProperty(), mybatisExportConfig.getBeanLocation());
            mybatisExportConfig.beanLocationProperty().bindBidirectional(beanLocationText.getTextField().textProperty());
            beanLocationText.onAction(event -> this.beanDirectoryScan(beanLocationText));
            MybatisExportItemHBox beanLocationHbox = new MybatisExportItemHBox("Bean 地址:", beanLocationText);
            validationSupport.registerValidator(beanLocationText.getTextField(), (Validator<String>) (c, s1) -> {
                boolean condition = mybatisExportConfig.modelEnableProperty().get() && StringUtils.isEmpty(s1);
                return ValidationResult.fromErrorIf(c, "Bean 地址必填", condition);
            });

            TextField beanPackageText = new TextField(mybatisExportConfig.getBeanPackage());
            mybatisExportConfig.beanPackageProperty().bindBidirectional(beanPackageText.textProperty());
            MybatisExportItemHBox beanPackageHbox = new MybatisExportItemHBox("Bean 包名:", beanPackageText);

            validationSupport.registerValidator(beanPackageText, (Validator<String>) (c, s1) -> {
                boolean condition = mybatisExportConfig.modelEnableProperty().get() && StringUtils.isEmpty(s1);
                return ValidationResult.fromErrorIf(c, "Bean 包名必填", condition);
            });

            TextField beanRootClassText = new TextField(mybatisExportConfig.getModelRootClass());
            beanRootClassText.setPromptText("非必填，生成的 Model 会继承该类");
            mybatisExportConfig.modelRootClassProperty().bindBidirectional(beanRootClassText.textProperty());
            MybatisExportItemHBox beanRootClassHbox = new MybatisExportItemHBox("Bean 父类:", beanRootClassText);

            Consumer<Boolean> beanEnableConsumer = newValue -> {
                beanLocationHbox.disable(!newValue);
                beanLocationText.disable(!newValue);
                beanPackageHbox.setDisable(!newValue);
                beanRootClassHbox.setDisable(!newValue);
                validationSupport.revalidate();
            };
            mybatisExportConfig.modelEnableProperty()
                    .addListener((observable, oldValue, newValue) -> beanEnableConsumer.accept(newValue));
            beanEnableConsumer.accept(mybatisExportConfig.isModelEnable());

            FileSelectTextToggleHBox mapperLocationText = new FileSelectTextToggleHBox("浏览",
                    mybatisExportConfig.mapperEnableProperty(), mybatisExportConfig.getMapperLocation());
            mybatisExportConfig.mapperLocationProperty().bindBidirectional(mapperLocationText.getTextField().textProperty());
            mapperLocationText.onAction(event -> this.beanDirectoryScan(mapperLocationText));
            MybatisExportItemHBox mapperLocationHbox = new MybatisExportItemHBox("Mapper 地址:", mapperLocationText);
            validationSupport.registerValidator(mapperLocationText.getTextField(), (Validator<String>) (c, s1) -> {
                boolean condition = mybatisExportConfig.mapperEnableProperty().get() && StringUtils.isEmpty(s1);
                return ValidationResult.fromErrorIf(c, "Mapper 地址必填", condition);
            });

            TextField mapperPackageText = new TextField(mybatisExportConfig.getMapperPackage());
            mybatisExportConfig.mapperPackageProperty().bindBidirectional(mapperPackageText.textProperty());
            MybatisExportItemHBox mapperPackageHbox = new MybatisExportItemHBox("Mapper 包名:", mapperPackageText);
            validationSupport.registerValidator(mapperPackageText, (Validator<String>) (c, s1) -> {
                boolean condition = mybatisExportConfig.mapperEnableProperty().get() && StringUtils.isEmpty(s1);
                return ValidationResult.fromErrorIf(c, "Mapper 包名必填", condition);
            });

            TextField mapperRootInterfaceText = new TextField(mybatisExportConfig.getMapperRootInterface());
            mapperRootInterfaceText.setPromptText("非必填，生成的 Mapper 会继承该接口");
            mybatisExportConfig.mapperRootInterfaceProperty().bindBidirectional(mapperRootInterfaceText.textProperty());
            MybatisExportItemHBox mapperRootInterfaceHbox = new MybatisExportItemHBox("Mapper 父接口:",
                    mapperRootInterfaceText);

            Consumer<Boolean> mapperEnableConsumer = newValue -> {
                mapperLocationHbox.disable(!newValue);
                mapperLocationText.disable(!newValue);
                mapperPackageHbox.setDisable(!newValue);
                mapperRootInterfaceHbox.setDisable(!newValue);
                validationSupport.revalidate();
            };
            mybatisExportConfig.mapperEnableProperty()
                    .addListener((observable, oldValue, newValue) -> mapperEnableConsumer.accept(newValue));
            mapperEnableConsumer.accept(mybatisExportConfig.isMapperEnable());

            FileSelectTextToggleHBox xmlLocationText = new FileSelectTextToggleHBox("浏览",
                    mybatisExportConfig.xmlEnableProperty(), mybatisExportConfig.getMapperXmlLocation());
            mybatisExportConfig.mapperXmlLocationProperty().bindBidirectional(xmlLocationText.getTextField().textProperty());
            xmlLocationText.onAction(event -> this.beanDirectoryScan(xmlLocationText));
            MybatisExportItemHBox xmlLocationHbox = new MybatisExportItemHBox("Xml 地址:", xmlLocationText);
            validationSupport.registerValidator(xmlLocationText.getTextField(), (Validator<String>) (c, s1) -> {
                boolean condition = mybatisExportConfig.xmlEnableProperty().get() && StringUtils.isEmpty(s1);
                return ValidationResult.fromErrorIf(c, "Xml 地址必填", condition);
            });

            Consumer<Boolean> xmlEnableConsumer = newValue -> {
                xmlLocationHbox.disable(!newValue);
                xmlLocationText.disable(!newValue);
                validationSupport.revalidate();
            };
            mybatisExportConfig.xmlEnableProperty()
                    .addListener((observable, oldValue, newValue) -> xmlEnableConsumer.accept(newValue));
            xmlEnableConsumer.accept(mybatisExportConfig.isXmlEnable());

            TextField globalIgnoreFieldText = new TextField();
            globalIgnoreFieldText.setPromptText("逗号隔开");
            mybatisExportConfig.globalIgnoreFieldProperty().bindBidirectional(globalIgnoreFieldText.textProperty());
            MybatisExportItemHBox globalIgnoreFieldHbox = new MybatisExportItemHBox("全局忽略字段:", globalIgnoreFieldText);

            // 选择语言时，自动设置 bean 和 mapper 的地址
            languageComboBox.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> {
                if (StringUtils.isEmpty(mybatisExportConfig.getMapperXmlLocation()) && !"src/main/resources/mapper".equals(mybatisExportConfig.getMapperXmlLocation())) {
                    mybatisExportConfig.setMapperXmlLocation("src/main/resources/mapper");
                }
                switch (newValue) {
                    case Java -> {
                        if (StringUtils.isEmpty(mybatisExportConfig.getBeanLocation()) || "src/main/kotlin".equals(mybatisExportConfig.getBeanLocation())) {
                            mybatisExportConfig.setBeanLocation("src/main/java");
                        }
                        if (StringUtils.isEmpty(mybatisExportConfig.getMapperLocation()) || "src/main/kotlin".equals(mybatisExportConfig.getMapperLocation())) {
                            mybatisExportConfig.setMapperLocation("src/main/java");
                        }
                    }
                    case Kotlin -> {
                        if (StringUtils.isEmpty(mybatisExportConfig.getBeanLocation()) || "src/main/java".equals(mybatisExportConfig.getBeanLocation())) {
                            mybatisExportConfig.setBeanLocation("src/main/kotlin");
                        }
                        if (StringUtils.isEmpty(mybatisExportConfig.getMapperLocation()) || "src/main/java".equals(mybatisExportConfig.getMapperLocation())) {
                            mybatisExportConfig.setMapperLocation("src/main/kotlin");
                        }
                    }
                }
            });
            languageComboBox.setValue(mybatisExportConfig.getLanguage());
            mybatisExportConfig.languageProperty().bindBidirectional(languageComboBox.valueProperty());

            frameworkComboBox.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> {
                if (mybatisExportConfig.getMapperRootInterface() != null && !"com.mybatisflex.core.BaseMapper".equals(mybatisExportConfig.getMapperRootInterface())) {
                    currentMapperRootInterface = mybatisExportConfig.getMapperRootInterface();
                }
                switch (newValue) {
                    case OFFICIAL -> {
                        borderPane.setCenter(this.officePanel(mybatisExportConfig));
                        mybatisExportConfig.setXmlEnable(true);
                        mybatisExportConfig.setMapperRootInterface(null);
                    }
                    case MYBATIS_FLEX -> {
                        borderPane.setCenter(this.mybatisFlexPanel(mybatisExportConfig));
                        mybatisExportConfig.setXmlEnable(false);
                        mybatisExportConfig.setMapperRootInterface(Objects.requireNonNullElse(currentMapperRootInterface, "com.mybatisflex.core.BaseMapper"));
                    }
                }
            });
            frameworkComboBox.setValue(mybatisExportConfig.getFrameworkType());
            mybatisExportConfig.frameworkTypeProperty().bindBidirectional(frameworkComboBox.valueProperty());

            hBoxListView.getItems().addAll(configNameHbox, authorHbox, writeFileHbox, projectDirHbox, projectNameHbox
                    , languageHbox, frameworkHbox,
                    beanLocationHbox, beanPackageHbox, beanRootClassHbox, mapperLocationHbox, mapperPackageHbox,
                    mapperRootInterfaceHbox, xmlLocationHbox, globalIgnoreFieldHbox);

            return splitPane;
        });
    }

    public void valid(String configName) {
        Assert.isTrue(StringUtils.isNotEmpty(configName), "配置名称必填", NodeConstants.primaryStage);
        if (configNameValidationMap.get(configName).isInvalid()) {
            Toast.makeTextDefault(NodeConstants.primaryStage, "必填项不能为空");
        }
    }

    //-------------------------文件夹浏览------------------------------------------------------------------------------//

    /**
     * 选择文件夹时，记录上次记录的选择的文件夹，方便用户选择
     */
    private String baseDir;

    /**
     * bean 文件夹选择器
     */
    public void beanDirectoryScan(FileSelectTextToggleHBox fileSelectTextHBox) {
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ?
                null : this.baseDir);
        if (directory != null) {
            final String path = directory.getPath().replace(StrUtil.BACKSLASH, StrUtil.SLASH);
            fileSelectTextHBox.setText(path);
            this.baseDir = path;
        }
    }

    /**
     * bean 文件夹选择器
     */
    public void beanDirectoryScan(FileSelectTextHBox fileSelectTextHBox) {
        File directory = FileDirChooserFactory.createDirectoryScan(null, !StringUtils.isNotEmpty(this.baseDir) ?
                null : this.baseDir);
        if (directory != null) {
            final String path = directory.getPath().replace(StrUtil.BACKSLASH, StrUtil.SLASH);
            fileSelectTextHBox.setText(path);
            this.baseDir = path;
        }
    }

    private MybatisExportConfig getCurrentConfig() {
        final MybatisExportGroupItemHBox selectedItem =
                linkageBorderPane.getGroupLeftListView().getSelectionModel().getSelectedItem();
        Assert.isTrue(null != selectedItem, "请先新增配置", NodeConstants.primaryStage);
        assert selectedItem != null;
        return selectedItem.getConfig();
    }

    private void setJavaClient(JFXComboBox<JavaClientTypeEnum> javaClientTypeComboBox, TargetNameEnum newValue,
                               JavaClientTypeEnum value) {
        switch (newValue) {
            case Mybatis3 -> {
                javaClientTypeComboBox.setDisable(false);
                javaClientTypeComboBox.getItems().clear();
                javaClientTypeComboBox.getItems().addAll(JavaClientTypeEnum.values());
                javaClientTypeComboBox.getSelectionModel().select(value);
            }
            case MyBatis3Simple -> {
                javaClientTypeComboBox.setDisable(false);
                javaClientTypeComboBox.getItems().clear();
                javaClientTypeComboBox.getItems().addAll(JavaClientTypeEnum.JAVA_CLIENT_TYPE_ENUMS);
                javaClientTypeComboBox.getSelectionModel().select(value);
            }
            default -> {
                javaClientTypeComboBox.setDisable(true);
                javaClientTypeComboBox.getSelectionModel().clearSelection();
            }
        }
    }

    private AnchorPane officePanel(MybatisExportConfig mybatisExportConfig) {
// 底下的配置
        AnchorPane anchorPane = new AnchorPane();

        final MybatisExportConfig.MybatisOfficialExportConfig mybatisOfficialExportConfig =
                mybatisExportConfig.getMybatisExportConfig();
        JFXCheckBox userJava8CheckBox = new JFXCheckBox("支持 java8");
        userJava8CheckBox.setSelected(mybatisOfficialExportConfig.isUserJava8());
        userJava8CheckBox.setLayoutX(27);
        userJava8CheckBox.setLayoutY(51);
        mybatisOfficialExportConfig.userJava8Property().bindBidirectional(userJava8CheckBox.selectedProperty());

        JFXCheckBox useBigDecimalCheckBox = new JFXCheckBox("使用 BigDecimal");
        useBigDecimalCheckBox.setSelected(mybatisOfficialExportConfig.isUseBigDecimal());
        useBigDecimalCheckBox.setLayoutX(262);
        useBigDecimalCheckBox.setLayoutY(88);
        mybatisOfficialExportConfig.useBigDecimalProperty().bindBidirectional(useBigDecimalCheckBox.selectedProperty());

        JFXCheckBox useJpaAnnotationCheckBox = new JFXCheckBox("使用 JPA 注解");
        useJpaAnnotationCheckBox.setSelected(mybatisOfficialExportConfig.isUseJpaAnnotation());
        useJpaAnnotationCheckBox.setLayoutX(300);
        useJpaAnnotationCheckBox.setLayoutY(88);
        mybatisOfficialExportConfig.useJpaAnnotationProperty().bindBidirectional(useJpaAnnotationCheckBox.selectedProperty());

        JFXCheckBox useCommentCheckBox = new JFXCheckBox("启用 Validation 注解");
        useCommentCheckBox.setSelected(mybatisOfficialExportConfig.isUseValidationAnnotation());
        useCommentCheckBox.setLayoutX(300);
        useCommentCheckBox.setLayoutY(51);
        mybatisOfficialExportConfig.useValidationAnnotationProperty().bindBidirectional(useCommentCheckBox.selectedProperty());

        JFXCheckBox useLombokGetSetCheckBox = new JFXCheckBox("启用 lombokGetSet 注解");
        useLombokGetSetCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokGetSet());
        useLombokGetSetCheckBox.setLayoutX(27);
        useLombokGetSetCheckBox.setLayoutY(88);
        mybatisOfficialExportConfig.useLombokGetSetProperty().bindBidirectional(useLombokGetSetCheckBox.selectedProperty());

        JFXCheckBox useLombokBuilderCheckBox = new JFXCheckBox("启用 lombokBuilder 注解");
        useLombokBuilderCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokBuilder());
        useLombokBuilderCheckBox.setLayoutX(27);
        useLombokBuilderCheckBox.setLayoutY(123);
        mybatisOfficialExportConfig.useLombokBuilderProperty().bindBidirectional(useLombokBuilderCheckBox.selectedProperty());

        Label targetNameLabel = new Label("targetName:");
        targetNameLabel.setLayoutX(27);
        targetNameLabel.setLayoutY(15);

        Label javaClientTypeLabel = new Label("javaClientType:");
        javaClientTypeLabel.setLayoutX(300);
        javaClientTypeLabel.setLayoutY(15);
        final JFXComboBox<JavaClientTypeEnum> javaClientTypeComboBox = new JFXComboBox<>();
        javaClientTypeComboBox.setPrefWidth(160);
        javaClientTypeComboBox.setLayoutX(400);
        javaClientTypeComboBox.setLayoutY(10);
        this.setJavaClient(javaClientTypeComboBox, mybatisOfficialExportConfig.getTargetName(),
                mybatisOfficialExportConfig.getJavaClientType());
        javaClientTypeComboBox.valueProperty().bindBidirectional(mybatisOfficialExportConfig.javaClientTypeProperty());

        // 高级设置按钮
        Button advanceSetButton = new Button();
        advanceSetButton.setOnAction(event -> mybatisAdvanceSetController.openAdvanceSetStage(mybatisExportConfig));
        advanceSetButton.setGraphic(new FontIcon("unil-setting:16:BLUE"));
        advanceSetButton.setLayoutX(600);
        advanceSetButton.setLayoutY(10);

        final JFXComboBox<TargetNameEnum> targetNameJFXComboBox =
                new JFXComboBox<>(FXCollections.observableArrayList(TargetNameEnum.values()));
        targetNameJFXComboBox.setPrefWidth(160);
        targetNameJFXComboBox.setLayoutX(115);
        targetNameJFXComboBox.setLayoutY(10);
        targetNameJFXComboBox.setValue(mybatisOfficialExportConfig.getTargetName());
        targetNameJFXComboBox.valueProperty().bindBidirectional(mybatisOfficialExportConfig.targetNameProperty());
        targetNameJFXComboBox.getSelectionModel().selectedItemProperty()
                .addListener((observable, oldValue,
                              newValue) -> this.setJavaClient(javaClientTypeComboBox, newValue,
                        JavaClientTypeEnum.XMLMAPPER));

        anchorPane.getChildren().addAll(userJava8CheckBox, useJpaAnnotationCheckBox, useCommentCheckBox,
                useLombokGetSetCheckBox, useLombokBuilderCheckBox, targetNameLabel, javaClientTypeLabel,
                javaClientTypeComboBox, targetNameJFXComboBox, advanceSetButton);
        return anchorPane;
    }

    private AnchorPane mybatisFlexPanel(MybatisExportConfig mybatisExportConfig) {
        // 底下的配置
        AnchorPane anchorPane = new AnchorPane();

        final MybatisExportConfig.MybatisOfficialExportConfig mybatisOfficialExportConfig =
                mybatisExportConfig.getMybatisExportConfig();
        JFXCheckBox userJava8CheckBox = new JFXCheckBox("支持 java8");
        userJava8CheckBox.setSelected(mybatisOfficialExportConfig.isUserJava8());
        userJava8CheckBox.setLayoutX(27);
        userJava8CheckBox.setLayoutY(51);
        mybatisOfficialExportConfig.userJava8Property().bindBidirectional(userJava8CheckBox.selectedProperty());

        JFXCheckBox useBigDecimalCheckBox = new JFXCheckBox("使用 BigDecimal");
        useBigDecimalCheckBox.setSelected(mybatisOfficialExportConfig.isUseBigDecimal());
        useBigDecimalCheckBox.setLayoutX(262);
        useBigDecimalCheckBox.setLayoutY(88);
        mybatisOfficialExportConfig.useBigDecimalProperty().bindBidirectional(useBigDecimalCheckBox.selectedProperty());

        JFXCheckBox useCommentCheckBox = new JFXCheckBox("启用 Validation 注解");
        useCommentCheckBox.setSelected(mybatisOfficialExportConfig.isUseValidationAnnotation());
        useCommentCheckBox.setLayoutX(262);
        useCommentCheckBox.setLayoutY(51);
        mybatisOfficialExportConfig.useValidationAnnotationProperty().bindBidirectional(useCommentCheckBox.selectedProperty());

        JFXCheckBox useLombokGetSetCheckBox = new JFXCheckBox("启用 lombokGetSet 注解");
        useLombokGetSetCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokGetSet());
        useLombokGetSetCheckBox.setLayoutX(27);
        useLombokGetSetCheckBox.setLayoutY(88);
        mybatisOfficialExportConfig.useLombokGetSetProperty().bindBidirectional(useLombokGetSetCheckBox.selectedProperty());

        JFXCheckBox useLombokBuilderCheckBox = new JFXCheckBox("启用 lombokBuilder 注解");
        useLombokBuilderCheckBox.setSelected(mybatisOfficialExportConfig.isUseLombokBuilder());
        useLombokBuilderCheckBox.setLayoutX(27);
        useLombokBuilderCheckBox.setLayoutY(123);
        mybatisOfficialExportConfig.useLombokBuilderProperty().bindBidirectional(useLombokBuilderCheckBox.selectedProperty());

        // 高级设置按钮
        Button advanceSetButton = new Button();
        advanceSetButton.setOnAction(event -> mybatisAdvanceSetController.openAdvanceSetStage(mybatisExportConfig));
        advanceSetButton.setGraphic(new FontIcon("unil-setting:16:BLUE"));
        advanceSetButton.setLayoutX(600);
        advanceSetButton.setLayoutY(10);

        final JFXComboBox<TargetNameEnum> targetNameJFXComboBox =
                new JFXComboBox<>(FXCollections.observableArrayList(TargetNameEnum.values()));
        targetNameJFXComboBox.setPrefWidth(160);
        targetNameJFXComboBox.setLayoutX(115);
        targetNameJFXComboBox.setLayoutY(10);
        targetNameJFXComboBox.setValue(TargetNameEnum.MyBatis3Flex);
        targetNameJFXComboBox.valueProperty().bindBidirectional(mybatisOfficialExportConfig.targetNameProperty());
        targetNameJFXComboBox.setVisible(false);

        Label logicDeleteLabel = new Label("逻辑删除");
        logicDeleteLabel.setLayoutX(27);
        logicDeleteLabel.setLayoutY(15);
        TextField logicDeleteField = new TextField();
        logicDeleteField.setText(mybatisOfficialExportConfig.getLogicDeleteField());
        logicDeleteField.setPromptText("逻辑删除字段");
        logicDeleteField.textProperty().bindBidirectional(mybatisOfficialExportConfig.logicDeleteFieldProperty());
        logicDeleteField.setLayoutX(90);
        logicDeleteField.setLayoutY(10);

        Label versionLabel = new Label("乐观锁");
        versionLabel.setLayoutX(262);
        versionLabel.setLayoutY(15);
        TextField version = new TextField();
        version.setText(mybatisOfficialExportConfig.getVersion());
        version.setPromptText("乐观锁字段");
        version.textProperty().bindBidirectional(mybatisOfficialExportConfig.versionProperty());
        version.setLayoutX(320);
        version.setLayoutY(10);

        mybatisOfficialExportConfig.setTargetName(TargetNameEnum.MyBatis3Flex);

        anchorPane.getChildren().addAll(userJava8CheckBox, useCommentCheckBox, logicDeleteLabel,
                versionLabel, logicDeleteField, version,
                useLombokGetSetCheckBox, useLombokBuilderCheckBox, targetNameJFXComboBox, advanceSetButton);
        return anchorPane;
    }
}
