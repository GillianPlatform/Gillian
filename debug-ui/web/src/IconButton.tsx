import { ButtonOverrideProps } from "@sedap/react";
import { VSCodeButton } from "@vscode/webview-ui-toolkit/react";

const IconButton: React.FC<ButtonOverrideProps> = ({
  disabled,
  onClick,
  className,
  style,
  children,
}) => {
  const props = {
    appearance: "icon",
    disabled,
    onClick,
    className,
    style: { pointerEvents: "all", ...style },
    children,
  } as const;
  return <VSCodeButton {...props} />;
};

export default IconButton;
