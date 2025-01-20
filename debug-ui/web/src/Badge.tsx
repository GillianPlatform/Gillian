import { ComponentOverrideProps } from "@sedap/react";
import { VSCodeBadge } from "@vscode/webview-ui-toolkit/react";

const Badge: React.FC<ComponentOverrideProps> = ({ className, style, children }) => {
  const props = { className, style, children } as const;
  return <VSCodeBadge {...props} />;
};

export default Badge;
