package com.alan344.bean.config;

import com.alan344.componet.LeftRightLinkageBorderPane;
import com.alibaba.fastjson2.annotation.JSONField;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;

/**
 * @author AlanSun
 * @date 2023/1/30 15:58
 */
@Getter
@Setter
public class ExtraTemplateFileGroupConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;

    private boolean isEnable;
    /**
     * 是否是系统内置的配置
     */
    private boolean isSystem;

    private Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList;

    @Override
    public ExtraTemplateFileGroupConfig clone() {
        try {
            ExtraTemplateFileGroupConfig clone = (ExtraTemplateFileGroupConfig) super.clone();
            // TODO: copy mutable state here, so the clone can't change the internals of the original
            clone.setExtraTemplateFileConfigList(extraTemplateFileConfigList.stream().map(ExtraTemplateFileConfig::clone).toList());
            return clone;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public Collection<ExtraTemplateFileConfig> getList() {
        return this.extraTemplateFileConfigList;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public void setList(Collection list) {
        this.extraTemplateFileConfigList = list;
    }
}
