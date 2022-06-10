import React from 'react';

type Props = {
  children: React.ReactNode;
};

const Hero = ({ children }: Props) => (
  <div
    style={{
      width: '100%',
      height: '100%',
      display: 'flex',
      justifyContent: 'center',
      alignItems: 'center',
      flexDirection: 'column',
    }}
  >
    {children}
  </div>
);

export default Hero;
